-- | Lists in compiled code, made in the kernel's arena (see 'Arena').
--
-- A list is the address of its first node, or 0 for the empty list. A
-- node is the address of the next one (an i32), padded to 8 bytes, then
-- the element: a truth value or integer as an i32, a real number as an
-- f64, a complex number as two, a color as three bytes, a list as its
-- address, and a pair as its first part, then its second at the next
-- multiple of 8 bytes. (The arguments buffer holds a configuration's
-- lists the same way: see "Backend.Wasm".)
--
-- Each operation does what the LLVM backend's does: a list that doesn't
-- fit in the arena sets the overflow flag and stops short, and indexing
-- out of range sets the bad-index flag and gives zero.
module Backend.Wasm.List
  ( elemSize
  , nodeSize
  , pairSecond
  , elemBytes
  , nodeStride
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
  PairType a b -> (+) <$> pairSecond a <*> elemSize b
  _ -> Nothing

-- | Where a pair element's second part starts.
pairSecond :: TypeProxy t -> Maybe Int
pairSecond a = roundUp8 <$> elemSize a

-- | The size of a list node holding an element of the given type.
nodeSize :: TypeProxy t -> Maybe Int
nodeSize t = roundUp8 . (8 +) <$> elemSize t

elemBytes :: TypeProxy t -> Gen Int
elemBytes t = maybe (unsupported ("lists of " ++ showType t)) pure (elemSize t)

pairSecondOffset :: TypeProxy t -> Gen Int
pairSecondOffset a = maybe (unsupported ("lists of " ++ showType a)) pure (pairSecond a)

nodeStride :: TypeProxy t -> Gen Int
nodeStride t = maybe (unsupported ("lists of " ++ showType t)) pure (nodeSize t)

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

-- | A list being built, node by node: its head, and its last node.
data Builder = Builder { bHead :: Int, bLast :: Int }

newBuilder :: Gen Builder
newBuilder = do
  h <- newLocal I32
  l <- newLocal I32
  emit [i32Const 0, localSet h, i32Const 0, localSet l]
  pure (Builder h l)

-- | Put a new node (whose element is already stored) at the end.
append :: Builder -> Int -> [Instr]
append Builder{..} node =
  [ localGet node, i32Const 0, i32Store 0
  , localGet bLast, i32Eqz
  , IfElse [ localGet node, localSet bHead ]
           [ localGet bLast, localGet node, i32Store 0 ]
  , localGet node, localSet bLast ]

-- | Run the body for each node of the list in local @headL@, with the
-- node's address in a local, and a local to set (to 1) to stop early.
walkList :: Int -> (Int -> Int -> Gen ()) -> Gen ()
walkList headL body = do
  curr <- newLocal I32
  stop <- newLocal I32
  (_, b) <- capture (body curr stop)
  emit [ localGet headL, localSet curr, i32Const 0, localSet stop
       , Block [ Loop ( [ localGet curr, i32Eqz, localGet stop, i32Or, brIf 1 ]
                        ++ b
                        ++ [ localGet curr, i32Load 0, localSet curr, br 0 ] ) ] ]

-- | A list literal (its elements already computed): all its nodes at
-- once, or the empty list (and the overflow flag) if they don't fit.
listLiteral :: TypeProxy t -> [Loc] -> Gen Loc
listLiteral ty elems = do
  h <- newLocal I32
  emit [i32Const 0, localSet h]
  unless (null elems) $ do
    stride <- nodeStride ty
    let n = length elems
    p <- alloc (stride * n)
    node <- newLocal I32
    (_, fill) <- capture $ forM_ (zip [0 ..] elems) $ \(k, e) -> do
      let next | k == n - 1 = [i32Const 0]
               | otherwise = [localGet p, i32Const (fromIntegral ((k + 1) * stride)), i32Add]
      emit ( [ localGet p, i32Const (fromIntegral (k * stride)), i32Add, localSet node
             , localGet node ] ++ next ++ [ i32Store 0 ] )
      storeElem ty node 8 e
    emit [ localGet p, IfElse (fill ++ [localGet p, localSet h]) [] ]
  pure (LList h)

-- | The lists one after another, copied.
listJoin :: TypeProxy t -> [Loc] -> Gen Loc
listJoin ty lists = do
  stride <- nodeStride ty
  b <- newBuilder
  forM_ lists $ \case
    LList h -> walkList h $ \curr stop -> do
      new <- alloc stride
      (_, copy) <- capture $ do
        e <- loadElem ty curr 8
        storeElem ty new 8 e
        emit (append b new)
      emit [ localGet new, i32Eqz, IfElse [i32Const 1, localSet stop] copy ]
    l -> throwError ("internal error: joining a non-list: " ++ show l)
  pure (LList (bHead b))

-- | The elements for which the test is false (@remove@).
listFilter :: TypeProxy t -> Loc -> (Loc -> Gen Loc) -> Gen Loc
listFilter ty xs test = case xs of
  LList h -> do
    stride <- nodeStride ty
    b <- newBuilder
    walkList h $ \curr stop -> do
      e <- loadElem ty curr 8
      removeIt <- test e >>= boolLocal
      (_, keep) <- capture $ do
        new <- alloc stride
        (_, store) <- capture (storeElem ty new 8 e >> emit (append b new))
        emit [ localGet new, i32Eqz, IfElse [i32Const 1, localSet stop] store ]
      emit [ localGet removeIt, IfElse [] keep ]
    pure (LList (bHead b))
  l -> throwError ("internal error: filtering a non-list: " ++ show l)

-- | Each element transformed.
listMap :: TypeProxy t1 -> TypeProxy t2 -> Loc -> (Loc -> Gen Loc) -> Gen Loc
listMap ty1 ty2 xs f = case xs of
  LList h -> do
    stride2 <- nodeStride ty2
    b <- newBuilder
    walkList h $ \curr stop -> do
      e <- loadElem ty1 curr 8
      out <- f e
      new <- alloc stride2
      (_, store) <- capture (storeElem ty2 new 8 out >> emit (append b new))
      emit [ localGet new, i32Eqz, IfElse [i32Const 1, localSet stop] store ]
    pure (LList (bHead b))
  l -> throwError ("internal error: mapping over a non-list: " ++ show l)

-- | The first element for which the test is true, or the default.
listFind :: TypeProxy t -> Loc -> Loc -> (Loc -> Gen Loc) -> Gen Loc
listFind ty xs dflt test = case xs of
  LList h -> do
    result <- freshLike dflt
    copyInto dflt result
    walkList h $ \curr stop -> do
      e <- loadElem ty curr 8
      found <- test e >>= boolLocal
      (_, take') <- capture (copyInto e result)
      emit [ localGet found, IfElse (take' ++ [i32Const 1, localSet stop]) [] ]
    pure result
  l -> throwError ("internal error: searching a non-list: " ++ show l)

listLength :: Loc -> Gen Loc
listLength = \case
  LList h -> do
    n <- newLocal I32
    emit [i32Const 0, localSet n]
    walkList h $ \_ _ -> emit [localGet n, i32Const 1, i32Add, localSet n]
    pure (LInt n)
  l -> throwError ("internal error: the length of a non-list: " ++ show l)

-- | 1-based indexing: from the front for i >= 1, from the back for
-- i <= 0, or (cyclic) wrapping around. Out of range sets the bad-index
-- flag, and gives zero.
listIndex :: TypeProxy t -> Bool -> Loc -> Loc -> Gen Loc
listIndex ty cyclic xs ix = case (xs, ix) of
  (LList h, LInt i) -> do
    Arena{..} <- getArena
    len <- listLength xs >>= \case { LInt n -> pure n; l -> throwError (show l) }
    step <- newLocal I32
    if cyclic
      -- ((i - 1) rem len + len) rem len, unless the list is empty (when
      -- the walk below finds nothing, as the LLVM backend's does)
      then emit [ localGet len, i32Eqz
                , IfElse [ i32Const 0, localSet step ]
                    [ localGet i, i32Const 1, i32Sub, localGet len, i32RemS
                    , localGet len, i32Add, localGet len, i32RemS, localSet step ] ]
      else emit [ localGet len, localGet i, i32Add
                , localGet i, i32Const 1, i32Sub
                , localGet i, i32Const 0, i32LeS, select, localSet step ]
    curr <- newLocal I32
    emit [ localGet h, localSet curr
         , Block [ Loop [ localGet step, i32Eqz, localGet curr, i32Eqz, i32Or, brIf 1
                        , localGet step, i32Const 1, i32Sub, localSet step
                        , localGet curr, i32Load 0, localSet curr
                        , br 0 ] ] ]
    -- (a negative step walks to the end: out of range)
    result <- zeroOf ty
    (_, found) <- capture (loadElem ty curr 8 >>= (`copyInto` result))
    emit [ localGet curr, i32Eqz, localGet step, i32Const 0, i32LtS, i32Or
         , IfElse [ i32Const 1, localSet arBadIndex ] found ]
    pure result
  _ -> throwError ("internal error: indexing a non-list: " ++ show xs)

-- | The integers from lo to hi.
listRange :: Loc -> Loc -> Gen Loc
listRange lo hi = case (lo, hi) of
  (LInt a, LInt z) -> do
    stride <- nodeStride IntegerType
    b <- newBuilder
    i <- newLocal I32
    new <- newLocal I32
    (_, body) <- capture $ do
      p <- alloc stride
      emit [localGet p, localSet new]
      (_, store) <- capture (storeElem IntegerType new 8 (LInt i) >> emit (append b new))
      emit [ localGet new, i32Eqz, brIf 1 ]
      emit store
    emit [ localGet a, localSet i
         , Block [ Loop ( [ localGet i, localGet z, i32GtS, brIf 1 ]
                          ++ body
                          ++ [ localGet i, i32Const 1, i32Add, localSet i, br 0 ] ) ] ]
    pure (LList (bHead b))
  _ -> throwError ("internal error: a range of non-integers: " ++ show lo)

boolLocal :: Loc -> Gen Int
boolLocal = \case
  LBool b -> pure b
  l -> throwError ("internal error: a test that isn't true or false: " ++ show l)
