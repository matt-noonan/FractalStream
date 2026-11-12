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
  ) where

import FractalStream.Prelude

import Backend.Wasm.Encode
import Backend.Wasm.Gen
import Backend.Wasm.List (alloc, nodeStride, storeElem, newBuilder, append, Builder(..))

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
  stride <- nodeStride ty
  b <- newBuilder
  i <- newLocal I32
  cell <- newLocal I32
  (_, body) <- capture $ do
    new <- alloc stride
    (_, store) <- capture $ do
      emit [localGet base, localGet i, localGet step, i32Mul, i32Add, localSet cell]
      p <- cellAddress ty desc cell
      e <- loadTableElem ty p 0
      storeElem ty new 8 e
      emit (append b new)
    emit [ localGet new, i32Eqz, brIf 2 ]
    emit store
  emit [ localGet inRange
       , IfElse [ i32Const 0, localSet i
                , Block [ Loop ( [ localGet i, localGet count, i32GeS, brIf 1 ]
                                 ++ body
                                 ++ [ localGet i, i32Const 1, i32Add, localSet i, br 0 ] ) ] ]
                [ i32Const 1, localSet arBadIndex ] ]
  pure (LList (bHead b))

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
