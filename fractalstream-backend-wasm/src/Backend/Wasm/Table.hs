-- | Tables in compiled code (reading them; see "Language.Table").
--
-- A table is the address of its /descriptor/ (an i32), laid out as the
-- LLVM backend's ("Backend.LLVM.Table"), but with a 32-bit pointer:
--
-- > offset  0: i32    the address of the elements (laid out as "Language.Table" says)
-- > offset  4: i32    the number of dimensions, n
-- > offset  8: i32    frozen: nonzero if others may be looking at the table
-- > offset 16: f64    origin x, the center of cell (1, 1)
-- > offset 24: f64    origin y
-- > offset 32: f64    a cell's width
-- > offset 40: f64    a cell's height (rows go down)
-- > offset 48: i32[n] the size along each dimension
--
-- The engine writes a configuration's tables' descriptors into the
-- arguments buffer (see "Backend.Wasm"), and where the elements are: on
-- its own thread, the table's buffer; on a worker, its copy of the table.
-- Each operation does what the LLVM backend's does: an index out of range
-- sets the bad-index flag (and gives zero).
--
-- A tool's handler can also change tables, through the functions it
-- imports from module @tables@ (@libm/fstables.c@, which makes new tables
-- in its arena, after its lists). A table variable holds a descriptor; a
-- table that others may be looking at (a configuration value, or another
-- variable's too) is /frozen/, so writing it writes a copy ("thawed")
-- first, which the variable then holds. A write is skipped unless the
-- table is thawed: so a worker's copy of a configuration's table is never
-- written, even if copying it ran out of room.
module Backend.Wasm.Table
  ( descriptorBytes
  , tableRead
  , tablePoint
  , tableSliceRead
  , tableSize
  , tableContains
  , tableCellIndices
  , tableCellPoint
  , loadTableElem
  , storeTableElem
  , tableElemBytes
    -- * Writing (a tool's handler only)
  , tableWrite
  , tableWritePoint
  , tableWriteSlice
  , tableCopySlice
  , tableResize
  , tableAllocate
  , freezeTable
  , tableImports
  ) where

import FractalStream.Prelude

import Backend.Wasm.Encode
import Backend.Wasm.Gen
import Backend.Wasm.List (alloc, elemStride, storeElem, loadElem, newList, forIndex, slotOf, walkList)

import Language.Type
import Language.Table (elementSize, secondOffset)

-- | How many bytes a descriptor for a table of n dimensions takes.
descriptorBytes :: Int -> Int
descriptorBytes n = (48 + 4 * n + 7) `div` 8 * 8

tableElemBytes :: TypeProxy t -> Gen Int
tableElemBytes t = maybe (unsupported ("tables of " ++ showType t)) pure (elementSize t)

-- | Load a table's element at the address in local @p@ plus @off@, into
-- fresh locals.
loadTableElem :: TypeProxy t -> Int -> Int -> Gen Loc
loadTableElem t p off = case t of
  -- (a truth value is a byte: any nonzero one is true)
  BooleanType -> LBool <$> one I32 [i32Load8U off, i32Const 0, i32Ne]
  IntegerType -> LInt <$> one I32 [i32Load off]
  RealType -> LReal <$> one F64 [f64Load off]
  ComplexType -> LComplex <$> one F64 [f64Load off] <*> one F64 [f64Load (off + 8)]
  ColorType -> LColor <$> one I32 [i32Load8U off] <*> one I32 [i32Load8U (off + 1)]
                      <*> one I32 [i32Load8U (off + 2)]
  PairType a b -> do
    off2 <- maybe (unsupported ("tables of " ++ showType t)) pure (secondOffset a)
    LPair <$> loadTableElem a p off <*> loadTableElem b p (off + off2)
  _ -> unsupported ("tables of " ++ showType t)
  where
    one vt load = do
      l <- newLocal vt
      emit ([localGet p] ++ load ++ [localSet l])
      pure l

-- | Store a table's element at the address in local @p@ plus @off@.
storeTableElem :: TypeProxy t -> Int -> Int -> Loc -> Gen ()
storeTableElem t p off v = case (t, v) of
  (BooleanType, LBool x) -> emit [localGet p, localGet x, i32Store8 off]
  (IntegerType, LInt x) -> emit [localGet p, localGet x, i32Store off]
  (RealType, LReal x) -> emit [localGet p, localGet x, f64Store off]
  (ComplexType, LComplex x y) ->
    emit [localGet p, localGet x, f64Store off, localGet p, localGet y, f64Store (off + 8)]
  (ColorType, LColor r g b) ->
    emit [ localGet p, localGet r, i32Store8 off, localGet p, localGet g, i32Store8 (off + 1)
         , localGet p, localGet b, i32Store8 (off + 2) ]
  (PairType a b, LPair x y) -> do
    off2 <- maybe (unsupported ("tables of " ++ showType t)) pure (secondOffset a)
    storeTableElem a p off x
    storeTableElem b p (off + off2) y
  _ -> throwError ("internal error: storing a table element of the wrong shape: " ++ show v)

-- | A slice of a table, given its indices (one per dimension, each an
-- integer in a local, or a @*@): whether its integers are in range, its
-- first cell (counting cells from 0), and for each @*@ the number of
-- cells it spans and how far apart they are (each in a local).
sliceView :: Int -> [Maybe Int] -> Gen (Int, Int, [(Int, Int)])
sliceView desc ixs = do
  inRange <- newLocal I32
  base <- newLocal I32
  stride <- newLocal I32
  emit [i32Const 1, localSet inRange, i32Const 0, localSet base, i32Const 1, localSet stride]
  spans <- fmap catMaybes . forM (zip [0 ..] ixs) $ \(k, mi) -> do
    dim <- newLocal I32
    emit [localGet desc, i32Load (48 + 4 * k), localSet dim]
    r <- case mi of
      Nothing -> do
        count <- newLocal I32
        step <- newLocal I32
        emit [localGet dim, localSet count, localGet stride, localSet step]
        pure (Just (count, step))
      Just i -> do
        emit [ localGet inRange
             , localGet i, i32Const 1, i32GeS, i32And
             , localGet i, localGet dim, i32LeS, i32And, localSet inRange
             , localGet base, localGet i, i32Const 1, i32Sub, localGet stride, i32Mul, i32Add, localSet base ]
        pure Nothing
    emit [localGet stride, localGet dim, i32Mul, localSet stride]
    pure r
  pure (inRange, base, spans)

-- | The address of the cell with the number in local @cell@ (counting
-- from 0), into a fresh local.
cellAddress :: TypeProxy t -> Int -> Int -> Gen Int
cellAddress ty desc cell = do
  size <- tableElemBytes ty
  p <- newLocal I32
  emit [ localGet desc, i32Load 0, localGet cell, i32Const (fromIntegral size), i32Mul, i32Add, localSet p ]
  pure p

-- | A cell's element, by its 1-based indices; out of range sets the
-- bad-index flag, and gives zero.
tableRead :: TypeProxy t -> Int -> [Int] -> Gen Loc
tableRead ty desc ixs = do
  Arena{..} <- getArena
  (inRange, base, _) <- sliceView desc (map Just ixs)
  result <- zeroOfTable ty
  (_, found) <- capture (cellAddress ty desc base >>= \p -> loadTableElem ty p 0 >>= (`copyInto` result))
  emit [ localGet inRange, IfElse found [ i32Const 1, localSet arBadIndex ] ]
  pure result

-- | The column and row of the cell nearest a point (as
-- 'Language.Table.nearestCell'), into fresh locals.
pointIndices :: Int -> Int -> Int -> Gen (Int, Int)
pointIndices desc x y = do
  let nearest u = u ++ [ f64Const 0.500001, f64Add, f64Floor, i32TruncSatF64S, i32Const 1, i32Add ]
  j <- newLocal I32
  k <- newLocal I32
  emit ( nearest [ localGet x, localGet desc, f64Load 16, f64Sub, localGet desc, f64Load 32, f64Div ]
         ++ [localSet j]
         ++ nearest [ localGet desc, f64Load 24, localGet y, f64Sub, localGet desc, f64Load 40, f64Div ]
         ++ [localSet k] )
  pure (j, k)

-- | The cell nearest a point, at some indices in the dimensions after
-- the first two.
tablePoint :: TypeProxy t -> Int -> (Int, Int) -> [Int] -> Gen Loc
tablePoint ty desc (x, y) rest = do
  (j, k) <- pointIndices desc x y
  tableRead ty desc ([j, k] ++ rest)

-- | A slice with one @*@ as a list (its first two indices maybe from a
-- point); out of range sets the bad-index flag and gives the empty list.
tableSliceRead :: TypeProxy t -> Int -> Maybe (Int, Int) -> [Maybe Int] -> Gen Loc
tableSliceRead ty desc mz ixs = do
  Arena{..} <- getArena
  pointed <- forM mz $ \(x, y) -> pointIndices desc x y <&> \(j, k) -> [Just j, Just k]
  (inRange, base, spans) <- sliceView desc (fromMaybe [] pointed ++ ixs)
  (count, step) <- case spans of
    [s] -> pure s
    _ -> throwError "internal error: a slice read as a list has one `*`"
  stride <- elemStride ty
  out <- newLocal I32
  cell <- newLocal I32
  (_, readIt) <- capture $ do
    l <- newList ty count $ \lst -> forIndex count $ \i _ -> do
      emit [localGet base, localGet i, localGet step, i32Mul, i32Add, localSet cell]
      p <- cellAddress ty desc cell
      e <- loadTableElem ty p 0
      dst <- slotOf stride lst i
      storeElem ty dst 8 e
    emit [localGet l, localSet out]
  emit [ i32Const 0, localSet out
       , localGet inRange, IfElse readIt [ i32Const 1, localSet arBadIndex ] ]
  pure (LList out)

-- | A table's size: an integer, or nested pairs of them.
tableSize :: TypeProxy sz -> Int -> Gen Loc
tableSize sz0 desc = go sz0 0
  where
    go :: TypeProxy s -> Int -> Gen Loc
    go sz k = case sz of
      IntegerType -> dimension k
      PairType IntegerType rest -> LPair <$> dimension k <*> go rest (k + 1)
      _ -> throwError "internal error: a table's size doesn't fit its type"
    dimension k = do
      l <- newLocal I32
      emit [localGet desc, i32Load (48 + 4 * k), localSet l]
      pure (LInt l)

-- | Whether a point is on a 2-dimensional table (no index error).
tableContains :: Int -> (Int, Int) -> Gen Loc
tableContains desc (x, y) = do
  (j, k) <- pointIndices desc x y
  (inRange, _, _) <- sliceView desc [Just j, Just k]
  pure (LBool inRange)

-- | The indices of the cell nearest a point, in range or not.
tableCellIndices :: TypeProxy sz -> Int -> (Int, Int) -> Gen Loc
tableCellIndices sz desc (x, y) = do
  (j, k) <- pointIndices desc x y
  case sz of
    PairType IntegerType IntegerType -> pure (LPair (LInt j) (LInt k))
    _ -> throwError "internal error: a table's cell doesn't fit its type"

-- | The point at the center of a 2-dimensional table's cell.
tableCellPoint :: Int -> Int -> Int -> Gen Loc
tableCellPoint desc j k = do
  x <- newLocal F64
  y <- newLocal F64
  emit [ localGet desc, f64Load 16, localGet j, i32Const 1, i32Sub, f64ConvertI32S, localGet desc, f64Load 32, f64Mul, f64Add, localSet x
       , localGet desc, f64Load 24, localGet k, i32Const 1, i32Sub, f64ConvertI32S, localGet desc, f64Load 40, f64Mul, f64Sub, localSet y ]
  pure (LComplex x y)

-- | Fresh locals holding zero, for a table element.
zeroOfTable :: TypeProxy t -> Gen Loc
zeroOfTable t = case t of
  BooleanType -> LBool <$> zero I32
  IntegerType -> LInt <$> zero I32
  RealType -> LReal <$> zero F64
  ComplexType -> LComplex <$> zero F64 <*> zero F64
  ColorType -> LColor <$> zero I32 <*> zero I32 <*> zero I32
  PairType a b -> LPair <$> zeroOfTable a <*> zeroOfTable b
  _ -> unsupported ("tables of " ++ showType t)
  where
    zero vt = do
      l <- newLocal vt
      emit [if vt == I32 then i32Const 0 else f64Const 0, localSet l]
      pure l

------------------------------------------------------------------------
-- Writing

-- | What a tool imports from module @tables@ (after the math functions
-- and the autocolor lookup): the functions of @libm/fstables.c@.
tableImports :: [Import]
tableImports =
  [ Import "tables" "new" [I32, I32, I32, F64, F64, F64, F64, I32, I32, I32] [I32]
  , Import "tables" "thaw" [I32, I32, I32, I32] [I32]
  , Import "tables" "resize" [I32, I32, I32, I32, I32, I32, I32] [I32]
  , Import "tables" "copy" [I32, I32, I32, I32, I32, I32, I32, I32, I32, I32, I32] [I32] ]

tableFn :: String -> Int
tableFn f = length mathImports + 1 + fromMaybe 0 (lookup f (zip ["new", "thaw", "resize", "copy"] [0 ..]))

-- | Mark a table as shared (frozen), when another variable gets it too.
freezeTable :: Loc -> Gen ()
freezeTable = \case
  LTable d -> emit [ localGet d, IfElse [ localGet d, i32Const 1, i32Store 8 ] [] ]
  _ -> pure ()

-- | Make the table in a variable's local one the tool can write in place:
-- a copy of it, if it's frozen.
thaw :: TypeProxy t -> Int -> Gen ()
thaw ty slot = do
  Arena{..} <- getArena
  size <- tableElemBytes ty
  r <- newLocal I32
  d <- newLocal I32
  emit [ localGet slot, i32Const 0, i32Ne, localGet arOverflow, i32Eqz, i32And
       , IfElse [ localGet slot, i32Load 8
                , IfElse [ localGet arBump, i32Const 7, i32Add, i32Const (-8), i32And, localSet d
                         , i32Const (fromIntegral size), localGet slot, localGet arBump, localGet arEnd
                         , call (tableFn "thaw"), localSet r
                         , localGet r, i32Const 0, i32LeS
                         , IfElse [ i32Const 1, localSet arOverflow ]
                                  [ localGet d, localSet slot, localGet r, localSet arBump ] ] [] ] [] ]

-- | Whether a table can be written in place now: it's there, thawed, and
-- the arena hasn't run out.
writable :: Int -> Gen Int
writable slot = do
  Arena{..} <- getArena
  ok <- newLocal I32
  emit [ localGet slot
       , IfElse [ localGet slot, i32Load 8, i32Eqz, localGet arOverflow, i32Eqz, i32And, localSet ok ]
                [ i32Const 0, localSet ok ] ]
  pure ok

-- | Write a cell, by its 1-based indices; out of range sets the
-- bad-index flag.
tableWrite :: TypeProxy t -> Int -> [Int] -> Loc -> Gen ()
tableWrite ty slot ixs v = do
  Arena{..} <- getArena
  thaw ty slot
  ok <- writable slot
  (_, write) <- capture $ do
    (inRange, base, _) <- sliceView slot (map Just ixs)
    (_, store) <- capture (cellAddress ty slot base >>= \p -> storeTableElem ty p 0 v)
    emit [ localGet inRange, IfElse store [ i32Const 1, localSet arBadIndex ] ]
  emit [ localGet ok, IfElse write [] ]

-- | Write the cell nearest a point, at some indices in the dimensions
-- after the first two.
tableWritePoint :: TypeProxy t -> Int -> (Int, Int) -> [Int] -> Loc -> Gen ()
tableWritePoint ty slot (x, y) rest v = do
  thaw ty slot
  (j, k) <- pointIndices slot x y
  tableWrite ty slot ([j, k] ++ rest) v

-- | Write a slice with one @*@ from a list, one element for each of its
-- cells (its first two indices maybe from a point); out of range, or a
-- list of another length, sets the bad-index flag.
tableWriteSlice :: TypeProxy t -> Int -> Maybe (Int, Int) -> [Maybe Int] -> Loc -> Gen ()
tableWriteSlice ty slot mz ixs xs = do
  Arena{..} <- getArena
  h <- case xs of { LList l -> pure l; l -> throwError ("internal error: writing a non-list to a slice: " ++ show l) }
  thaw ty slot
  ok <- writable slot
  (_, write) <- capture $ do
    pointed <- forM mz $ \(x, y) -> pointIndices slot x y <&> \(j, k) -> [Just j, Just k]
    (inRange, base, spans) <- sliceView slot (fromMaybe [] pointed ++ ixs)
    (count, step) <- case spans of
      [sp] -> pure sp
      _ -> throwError "internal error: a list is written to a slice with one `*`"
    i <- newLocal I32
    cell <- newLocal I32
    (_, walk) <- capture $ walkList ty h $ \curr stop -> do
      (_, store) <- capture $ do
        e <- loadElem ty curr 8
        emit [ localGet base, localGet i, localGet step, i32Mul, i32Add, localSet cell ]
        p <- cellAddress ty slot cell
        storeTableElem ty p 0 e
        emit [ localGet i, i32Const 1, i32Add, localSet i ]
      emit [ localGet i, localGet count, i32GeS
           , IfElse [ i32Const 1, localSet arBadIndex, i32Const 1, localSet stop ] store ]
    emit [ localGet inRange
         , IfElse ([ i32Const 0, localSet i ] ++ walk
                   ++ [ localGet i, localGet count, i32Ne, IfElse [ i32Const 1, localSet arBadIndex ] [] ])
                  [ i32Const 1, localSet arBadIndex ] ]
  emit [ localGet ok, IfElse write [] ]

-- | Integers (in locals) as an array in the arena: its address (0, with
-- the overflow flag set, if it doesn't fit).
intArray :: [Int] -> Gen Int
intArray ls = do
  p <- alloc (4 * max 1 (length ls))
  (_, store) <- capture $ forM_ (zip [0 ..] ls) $ \(k, l) -> emit [ localGet p, localGet l, i32Store (4 * k) ]
  emit [ localGet p, IfElse store [] ]
  pure p

constant :: Integer -> Gen Int
constant c = do
  l <- newLocal I32
  emit [ i32Const c, localSet l ]
  pure l

-- | A slice's indices, as arrays of indices and of which are @*@s.
sliceArrays :: Int -> Maybe (Int, Int) -> [Maybe Int] -> Gen (Int, Int, Int)
sliceArrays desc mz ixs = do
  pointed <- forM mz $ \(x, y) -> pointIndices desc x y <&> \(j, k) -> [Just j, Just k]
  let all' = fromMaybe [] pointed ++ ixs
  zero <- constant 0
  one <- constant 1
  is <- intArray (map (fromMaybe zero) all')
  stars <- intArray (map (maybe one (const zero)) all')
  pure (length all', is, stars)

-- | Copy a slice of a table onto a slice of the table in a variable's
-- local (by the host's @copy@); a failure sets the bad-index flag.
tableCopySlice :: TypeProxy t -> Int -> (Maybe (Int, Int), [Maybe Int]) -> Int -> (Maybe (Int, Int), [Maybe Int]) -> Gen ()
tableCopySlice ty slot (mz, ixs) src (mz', ixs') = do
  Arena{..} <- getArena
  size <- tableElemBytes ty
  thaw ty slot
  ok <- writable slot
  r <- newLocal I32
  (_, write) <- capture $ do
    (n, dixs, dstars) <- sliceArrays slot mz ixs
    (m, sixs, sstars) <- sliceArrays src mz' ixs'
    emit [ localGet arOverflow, i32Eqz
         , IfElse [ i32Const (fromIntegral size), localGet slot, i32Const (fromIntegral n), localGet dixs, localGet dstars
                  , localGet src, i32Const (fromIntegral m), localGet sixs, localGet sstars
                  , localGet arBump, localGet arEnd, call (tableFn "copy"), localSet r
                  , localGet r, i32Eqz, IfElse [ i32Const 1, localSet arBadIndex ] []
                  , localGet r, i32Const 0, i32LtS, IfElse [ i32Const 1, localSet arOverflow ] [] ] [] ]
  emit [ localGet ok, IfElse write [] ]

-- | A fill value (if any) in the arena: its address, or 0 for none.
fillValue :: TypeProxy t -> Maybe Loc -> Gen Int
fillValue ty = \case
  Nothing -> constant 0
  Just v -> do
    size <- tableElemBytes ty
    p <- alloc size
    (_, store) <- capture (storeTableElem ty p 0 v)
    emit [ localGet p, IfElse store [] ]
    pure p

-- | Give a variable a new table, from one of the host's functions, which
-- makes it at the arena's next free address: given the arguments before
-- @bump@ and @end@, it gives the new free address, 0 if it didn't fit
-- (the overflow flag), or -1 for a bad size (the bad-index flag).
newTableBy :: Int -> String -> [Instr] -> Gen ()
newTableBy slot f args = do
  Arena{..} <- getArena
  r <- newLocal I32
  d <- newLocal I32
  emit [ localGet arOverflow, i32Eqz
       , IfElse ( [ localGet arBump, i32Const 7, i32Add, i32Const (-8), i32And, localSet d ]
                  ++ args ++ [ localGet arBump, localGet arEnd, call (tableFn f), localSet r
                  , localGet r, i32Const 0, i32GtS
                  , IfElse [ localGet d, localSet slot, localGet r, localSet arBump ]
                      [ localGet r, i32Eqz
                      , IfElse [ i32Const 1, localSet arOverflow ] [ i32Const 1, localSet arBadIndex ] ] ] ) [] ]

-- | @resize T to (sizes) [filled with v]@.
tableResize :: TypeProxy t -> Int -> [Int] -> Maybe Loc -> Gen ()
tableResize ty slot sizes fill = do
  size <- tableElemBytes ty
  dims <- intArray sizes
  f <- fillValue ty fill
  newTableBy slot "resize"
    [ i32Const (fromIntegral size), localGet slot, i32Const (fromIntegral (length sizes)), localGet dims, localGet f ]

-- | @reset T to (sizes) [filled with v]@, over the rectangle with the
-- given corners (top left, then bottom right).
tableAllocate :: TypeProxy t -> Int -> [Int] -> (Int, Int) -> (Int, Int) -> Maybe Loc -> Gen ()
tableAllocate ty slot sizes (x1, y1) (x2, y2) fill = do
  size <- tableElemBytes ty
  dims <- intArray sizes
  f <- fillValue ty fill
  newTableBy slot "new"
    [ i32Const (fromIntegral size), i32Const (fromIntegral (length sizes)), localGet dims
    , localGet x1, localGet y1, localGet x2, localGet y2, localGet f ]
