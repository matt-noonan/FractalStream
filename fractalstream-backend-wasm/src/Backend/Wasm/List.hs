-- | Lists in compiled code, made in the kernel's arena (see 'Arena').
--
-- A list is the address of one block, or 0 for the empty list. The block
-- is the list's length (an i32), padded to 8 bytes, then the elements, one
-- every 'elemStride' bytes: a truth value or integer as an i32, a real
-- number as an f64, a complex number as two, a color as three bytes, a
-- list as its address, a text as its address (see "Backend.Wasm.Text"),
-- and a pair as its first part, then its second at the next multiple of 8
-- bytes. So indexing and the length are O(1). Lists are never changed in
-- place. (The arguments buffer holds a configuration's lists the same way:
-- see "Backend.Wasm". The LLVM backend lays lists out alike.)
--
-- Each operation does what the LLVM backend's does: a list that doesn't
-- fit in the arena sets the overflow flag and stops short, and indexing
-- out of range sets the bad-index flag and gives zero.
module Backend.Wasm.List
  ( elemSize
  , elemStrideOf
  , pairSecond
  , elemBytes
  , elemStride
  , pairSecondOffset
  , zeroOf
  , loadElem
  , storeElem
  , listLiteral
  , listJoin
  , listFilter
  , listMap
  , listFind
  , listLength
  , listIndex
  , listRange
  , walkList
  , forIndex
  , slotOf
  , lengthOf
  , newList
  , alloc
  , allocDyn
  ) where

import FractalStream.Prelude

import Backend.Wasm.Encode
import Backend.Wasm.Gen

import Language.Type

roundUp8 :: Int -> Int
roundUp8 n = (n + 7) `div` 8 * 8

-- | The size of a list element of the given type (Nothing for the types
-- lists can't hold yet).
elemSize :: TypeProxy t -> Maybe Int
elemSize = \case
  BooleanType -> Just 4
  IntegerType -> Just 4
  RealType -> Just 8
  ComplexType -> Just 16
  ColorType -> Just 4
  ListType t -> elemSize t >> Just 4
  TextType -> Just 4
  PairType a b -> (+) <$> pairSecond a <*> elemSize b
  _ -> Nothing

-- | Where a pair element's second part starts.
pairSecond :: TypeProxy t -> Maybe Int
pairSecond a = roundUp8 <$> elemSize a

-- | The distance between a list's elements of the given type.
elemStrideOf :: TypeProxy t -> Maybe Int
elemStrideOf t = roundUp8 <$> elemSize t

elemBytes :: TypeProxy t -> Gen Int
elemBytes t = maybe (unsupported ("lists of " ++ showType t)) pure (elemSize t)

pairSecondOffset :: TypeProxy t -> Gen Int
pairSecondOffset a = maybe (unsupported ("lists of " ++ showType a)) pure (pairSecond a)

elemStride :: TypeProxy t -> Gen Int
elemStride t = maybe (unsupported ("lists of " ++ showType t)) pure (elemStrideOf t)

-- | Load an element at the address in local @p@ plus @off@, into fresh
-- locals.
loadElem :: TypeProxy t -> Int -> Int -> Gen Loc
loadElem t p off = case t of
  BooleanType -> LBool <$> one I32 (i32Load off)
  IntegerType -> LInt <$> one I32 (i32Load off)
  RealType -> LReal <$> one F64 (f64Load off)
  ComplexType -> LComplex <$> one F64 (f64Load off) <*> one F64 (f64Load (off + 8))
  ColorType -> LColor <$> one I32 (i32Load8U off) <*> one I32 (i32Load8U (off + 1))
                      <*> one I32 (i32Load8U (off + 2))
  ListType _ -> LList <$> one I32 (i32Load off)
  TextType -> LText <$> one I32 (i32Load off)
  PairType a b -> do
    off2 <- pairSecondOffset a
    LPair <$> loadElem a p off <*> loadElem b p (off + off2)
  _ -> unsupported ("lists of " ++ showType t)
  where
    one vt load = do
      l <- newLocal vt
      emit [localGet p, load, localSet l]
      pure l

-- | Store an element at the address in local @p@ plus @off@.
storeElem :: TypeProxy t -> Int -> Int -> Loc -> Gen ()
storeElem t p off v = case (t, v) of
  (BooleanType, LBool x) -> emit [localGet p, localGet x, i32Store off]
  (IntegerType, LInt x) -> emit [localGet p, localGet x, i32Store off]
  (RealType, LReal x) -> emit [localGet p, localGet x, f64Store off]
  (ComplexType, LComplex x y) ->
    emit [localGet p, localGet x, f64Store off, localGet p, localGet y, f64Store (off + 8)]
  (ColorType, LColor r g b) ->
    emit [ localGet p, localGet r, i32Store8 off, localGet p, localGet g, i32Store8 (off + 1)
         , localGet p, localGet b, i32Store8 (off + 2) ]
  (ListType _, LList x) -> emit [localGet p, localGet x, i32Store off]
  (TextType, LText x) -> emit [localGet p, localGet x, i32Store off]
  (PairType a b, LPair x y) -> do
    off2 <- pairSecondOffset a
    storeElem a p off x
    storeElem b p (off + off2) y
  _ -> throwError ("internal error: storing a list element of the wrong shape: " ++ show v)

-- | Fresh locals holding zero, for a value of the given type.
zeroOf :: TypeProxy t -> Gen Loc
zeroOf t = case t of
  BooleanType -> LBool <$> zero I32
  IntegerType -> LInt <$> zero I32
  RealType -> LReal <$> zero F64
  ComplexType -> LComplex <$> zero F64 <*> zero F64
  ColorType -> LColor <$> zero I32 <*> zero I32 <*> zero I32
  ListType _ -> LList <$> zero I32
  TextType -> LText <$> zero I32
  PairType a b -> LPair <$> zeroOf a <*> zeroOf b
  _ -> unsupported ("lists of " ++ showType t)
  where
    zero vt = do
      l <- newLocal vt
      emit [if vt == I32 then i32Const 0 else f64Const 0, localSet l]
      pure l

-- | Allocate @size@ bytes in the arena, into a fresh local: 0 (and the
-- overflow flag set) if they don't fit.
alloc :: Int -> Gen Int
alloc size = do
  Arena{..} <- getArena
  p <- newLocal I32
  emit [ localGet arBump, localSet p
       , localGet arBump, i32Const (fromIntegral size), i32Add, localGet arEnd, i32GtU
       , IfElse [ i32Const 1, localSet arOverflow, i32Const 0, localSet p ]
                [ localGet arBump, i32Const (fromIntegral size), i32Add, localSet arBump ] ]
  pure p

-- | Like 'alloc', for a byte count in a local (a multiple of 8). The count
-- is compared, not added, first: a huge one can't wrap around.
allocDyn :: Int -> Gen Int
allocDyn size = do
  Arena{..} <- getArena
  p <- newLocal I32
  emit [ localGet arBump, localSet p
       , localGet size, localGet arEnd, localGet arBump, i32Sub, i32GtU
       , IfElse [ i32Const 1, localSet arOverflow, i32Const 0, localSet p ]
                [ localGet arBump, localGet size, i32Add, localSet arBump ] ]
  pure p

-- | A loop over @k = 0 .. n - 1@ (@n@ a local), with @k@ in a local and a
-- local to set (to 1) to stop early.
forIndex :: Int -> (Int -> Int -> Gen ()) -> Gen ()
forIndex n body = do
  k <- newLocal I32
  stop <- newLocal I32
  (_, b) <- capture (body k stop)
  emit [ i32Const 0, localSet k, i32Const 0, localSet stop
       , Block [ Loop ( [ localGet k, localGet n, i32GeS, localGet stop, i32Or, brIf 1 ]
                        ++ b
                        ++ [ localGet k, i32Const 1, i32Add, localSet k, br 0 ] ) ] ]

-- | The slot of element @k@ (a local) of the list in local @h@, into a
-- fresh local: the address 8 bytes before the element (so the slot of
-- element 0 is the list itself), where 'loadElem' and 'storeElem' at
-- offset 8 find it.
slotOf :: Int -> Int -> Int -> Gen Int
slotOf stride h k = do
  s <- newLocal I32
  emit [ localGet h, localGet k, i32Const (fromIntegral stride), i32Mul, i32Add, localSet s ]
  pure s

-- | Run the body for each element of the list in local @h@, with the
-- element's slot (see 'slotOf') in a local, and a local to set (to 1) to
-- stop early.
walkList :: TypeProxy t -> Int -> (Int -> Int -> Gen ()) -> Gen ()
walkList ty h body = do
  stride <- elemStride ty
  n <- lengthOf h
  forIndex n $ \k stop -> do
    s <- slotOf stride h k
    body s stop

-- | The length of the list in local @h@, into a fresh local.
lengthOf :: Int -> Gen Int
lengthOf h = do
  n <- newLocal I32
  emit [ localGet h, IfElse [ localGet h, i32Load 0, localSet n ] [ i32Const 0, localSet n ] ]
  pure n

-- | A new list with room for @n@ elements (a local), into a fresh local:
-- @fill@ (given it) writes them, if @n > 0@ and it fits in the arena;
-- otherwise the empty list (with the overflow flag set if it didn't fit).
newList :: TypeProxy t -> Int -> (Int -> Gen ()) -> Gen Int
newList ty n fill = do
  stride <- elemStride ty
  h <- newLocal I32
  bytes <- newLocal I32
  (_, made) <- capture $ do
    emit [ localGet n, i32Const (fromIntegral stride), i32Mul, i32Const 8, i32Add, localSet bytes ]
    p <- allocDyn bytes
    (_, filled) <- capture (fill p)
    emit [ localGet p
         , IfElse ([ localGet p, localGet n, i32Store 0 ] ++ filled ++ [ localGet p, localSet h ]) [] ]
  emit [ i32Const 0, localSet h
       , localGet n, i32Const 0, i32GtS, IfElse made [] ]
  pure h

-- | A list literal (its elements already computed): one block, or the
-- empty list (and the overflow flag) if it doesn't fit.
listLiteral :: TypeProxy t -> [Loc] -> Gen Loc
listLiteral ty elems = do
  h <- newLocal I32
  emit [i32Const 0, localSet h]
  unless (null elems) $ do
    stride <- elemStride ty
    let n = length elems
    p <- alloc (8 + stride * n)
    (_, fill) <- capture $ forM_ (zip [0 ..] elems) $ \(k, e) -> storeElem ty p (8 + k * stride) e
    emit [ localGet p
         , IfElse ([ localGet p, i32Const (fromIntegral n), i32Store 0 ] ++ fill ++ [localGet p, localSet h]) [] ]
  pure (LList h)

-- | The lists one after another, copied into one new list.
listJoin :: TypeProxy t -> [Loc] -> Gen Loc
listJoin ty lists = do
  stride <- elemStride ty
  hs <- forM lists $ \case
    LList h -> pure h
    l -> throwError ("internal error: joining a non-list: " ++ show l)
  lens <- mapM lengthOf hs
  total <- newLocal I32
  emit ([i32Const 0] ++ concat [ [localGet n, i32Add] | n <- lens ] ++ [localSet total])
  out <- newList ty total $ \p -> do
    at <- newLocal I32
    emit [i32Const 0, localSet at]
    forM_ hs $ \h -> walkList ty h $ \src _ -> do
      e <- loadElem ty src 8
      dst <- slotOf stride p at
      storeElem ty dst 8 e
      emit [localGet at, i32Const 1, i32Add, localSet at]
  pure (LList out)

-- | The elements for which the test is false (@remove@): a list with room
-- for all of them, and the length of what was kept (the empty list if
-- nothing was).
listFilter :: TypeProxy t -> Loc -> (Loc -> Gen Loc) -> Gen Loc
listFilter ty xs test = case xs of
  LList h -> do
    stride <- elemStride ty
    n <- lengthOf h
    kept <- newLocal I32
    emit [i32Const 0, localSet kept]
    out <- newList ty n $ \p -> do
      walkList ty h $ \src _ -> do
        e <- loadElem ty src 8
        removeIt <- test e >>= boolLocal
        (_, keep) <- capture $ do
          dst <- slotOf stride p kept
          storeElem ty dst 8 e
          emit [localGet kept, i32Const 1, i32Add, localSet kept]
        emit [ localGet removeIt, IfElse [] keep ]
      emit [ localGet p, localGet kept, i32Store 0 ]
    emit [ localGet kept, i32Eqz, IfElse [i32Const 0, localSet out] [] ]
    pure (LList out)
  l -> throwError ("internal error: filtering a non-list: " ++ show l)

-- | Each element transformed.
listMap :: TypeProxy t1 -> TypeProxy t2 -> Loc -> (Loc -> Gen Loc) -> Gen Loc
listMap ty1 ty2 xs f = case xs of
  LList h -> do
    stride1 <- elemStride ty1
    stride2 <- elemStride ty2
    n <- lengthOf h
    out <- newList ty2 n $ \p -> forIndex n $ \k _ -> do
      src <- slotOf stride1 h k
      e <- loadElem ty1 src 8
      v <- f e
      dst <- slotOf stride2 p k
      storeElem ty2 dst 8 v
    pure (LList out)
  l -> throwError ("internal error: mapping over a non-list: " ++ show l)

-- | The first element for which the test is true, or the default.
listFind :: TypeProxy t -> Loc -> Loc -> (Loc -> Gen Loc) -> Gen Loc
listFind ty xs dflt test = case xs of
  LList h -> do
    result <- freshLike dflt
    copyInto dflt result
    walkList ty h $ \curr stop -> do
      e <- loadElem ty curr 8
      found <- test e >>= boolLocal
      (_, take') <- capture (copyInto e result)
      emit [ localGet found, IfElse (take' ++ [i32Const 1, localSet stop]) [] ]
    pure result
  l -> throwError ("internal error: searching a non-list: " ++ show l)

listLength :: Loc -> Gen Loc
listLength = \case
  LList h -> LInt <$> lengthOf h
  l -> throwError ("internal error: the length of a non-list: " ++ show l)

-- | 1-based indexing: from the front for i >= 1, from the back for
-- i <= 0, or (cyclic) wrapping around. Out of range (and any index of the
-- empty list) sets the bad-index flag, and gives zero.
listIndex :: TypeProxy t -> Bool -> Loc -> Loc -> Gen Loc
listIndex ty cyclic xs ix = case (xs, ix) of
  (LList h, LInt i) -> do
    Arena{..} <- getArena
    stride <- elemStride ty
    len <- lengthOf h
    step <- newLocal I32
    ok <- newLocal I32
    if cyclic
      -- ((i - 1) rem len + len) rem len, unless the list is empty
      then emit [ localGet len, i32Eqz
                , IfElse [ i32Const 0, localSet step, i32Const 0, localSet ok ]
                    [ localGet i, i32Const 1, i32Sub, localGet len, i32RemS
                    , localGet len, i32Add, localGet len, i32RemS, localSet step
                    , i32Const 1, localSet ok ] ]
      else emit [ localGet len, localGet i, i32Add
                , localGet i, i32Const 1, i32Sub
                , localGet i, i32Const 0, i32LeS, select, localSet step
                -- 0 <= step < len, as one unsigned comparison
                , localGet step, localGet len, i32LtU, localSet ok ]
    result <- zeroOf ty
    (_, found) <- capture (slotOf stride h step >>= \s -> loadElem ty s 8 >>= (`copyInto` result))
    emit [ localGet ok, IfElse found [ i32Const 1, localSet arBadIndex ] ]
    pure result
  _ -> throwError ("internal error: indexing a non-list: " ++ show xs)

-- | The integers from lo to hi.
listRange :: Loc -> Loc -> Gen Loc
listRange lo hi = case (lo, hi) of
  (LInt a, LInt z) -> do
    stride <- elemStride IntegerType
    n <- newLocal I32
    emit [ localGet z, localGet a, i32Sub, i32Const 1, i32Add, localSet n ]
    out <- newList IntegerType n $ \p -> forIndex n $ \k _ -> do
      v <- newLocal I32
      emit [ localGet a, localGet k, i32Add, localSet v ]
      dst <- slotOf stride p k
      storeElem IntegerType dst 8 (LInt v)
    pure (LList out)
  _ -> throwError ("internal error: a range of non-integers: " ++ show lo)

boolLocal :: Loc -> Gen Int
boolLocal = \case
  LBool b -> pure b
  l -> throwError ("internal error: a test that isn't true or false: " ++ show l)
