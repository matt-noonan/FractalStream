-- | Lists and tables in a kernel (see "Backend.WGSL.Runtime" for their
-- layout), operation by operation as "Backend.Wasm.List" and
-- "Backend.Wasm.Table" do them, so the backends compute alike: a list that
-- doesn't fit in the pixel's arena sets @overflow@ and is the empty list;
-- an index out of range sets @badIndex@ and gives zero (or the empty
-- list).
module Backend.WGSL.Data
  ( elemWords
  , zeroOf
    -- * Lists
  , loadElem
  , storeElem
  , listLiteral
  , listAppend
  , listJoin
  , listFilter
  , listMap
  , listFind
  , listLength
  , listIndex
  , listRange
  , walkList
    -- * Tables
  , tableRead
  , tablePoint
  , tableSliceRead
  , tableSize
  , tableContains
  , tableCellIndices
  , tableCellPoint
    -- * Constants
  , realLiteral
  ) where

import FractalStream.Prelude

import Backend.WGSL.Gen

import Language.Type
import Language.Table (elementSize, secondOffset)

import GHC.Float (castFloatToWord32, double2Float, float2Double)
import Numeric (showHex)

-- | How many words a list element of the given type takes (Nothing for
-- the types a kernel's lists can't hold).
elemWords :: TypeProxy t -> Maybe Int
elemWords = \case
  BooleanType -> Just 1
  IntegerType -> Just 1
  RealType -> Just 2
  ComplexType -> Just 4
  ColorType -> Just 1
  ListType t -> elemWords t >> Just 1
  PairType a b -> (+) <$> elemWords a <*> elemWords b
  _ -> Nothing

stride :: TypeProxy t -> Gen Int
stride t = maybe (unsupported ("lists of " ++ showType t)) pure (elemWords t)

-- | An address plus some words.
at :: String -> Int -> String
at a 0 = a
at a k = "(" ++ a ++ " + " ++ show k ++ "u)"

-- | A double as an R, exactly as 'Backend.WGSL.Code' passes arguments.
realLiteral :: Double -> String
realLiteral x =
  let (hi, lo) | isNaN x || isInfinite x = (double2Float x, 0)
               | otherwise = let h = double2Float x in (h, double2Float (x - float2Double h))
  in "rbits(0x" ++ showHex (castFloatToWord32 hi) "u, 0x" ++ showHex (castFloatToWord32 lo) "u)"

------------------------------------------------------------------------
-- Elements

-- | The element at this word address.
loadElem :: TypeProxy t -> String -> Gen Loc
loadElem t a = case t of
  BooleanType -> letOf LBool ("lw(" ++ a ++ ") != 0u")
  IntegerType -> letOf LInt ("bitcast<i32>(lw(" ++ a ++ "))")
  RealType -> letOf LReal (real a)
  ComplexType -> letOf LComplex ("C(" ++ real a ++ ", " ++ real (at a 2) ++ ")")
  ColorType -> letOf LColor ("cunpack(lw(" ++ a ++ "))")
  -- (whoever holds it now shares it with the list it's in)
  ListType _ -> do
    l <- letOf LList ("lw(" ++ a ++ ")")
    case l of { LList h -> emit ("lshare(" ++ h ++ ");"); _ -> pure () }
    pure l
  PairType x y -> do
    w <- stride x
    LPair <$> loadElem x a <*> loadElem y (at a w)
  _ -> unsupported ("lists of " ++ showType t)
  where real b = "rbits(lw(" ++ b ++ "), lw(" ++ at b 1 ++ "))"

-- | Store an element at this word address (in the arena).
storeElem :: TypeProxy t -> String -> Loc -> Gen ()
storeElem t a v0 = force v0 >>= \v -> case (t, v) of
  (BooleanType, LBool b) -> store a ("select(0u, 1u, " ++ b ++ ")")
  (IntegerType, LInt i) -> store a ("bitcast<u32>(" ++ i ++ ")")
  (RealType, LReal r) -> real a r
  (ComplexType, LComplex c) -> real a (c ++ ".re") >> real (at a 2) (c ++ ".im")
  (ColorType, LColor c) -> store a ("cpack(" ++ c ++ ")")
  (ListType _, LList h) -> store a h
  (PairType x y, LPair p q) -> do
    w <- stride x
    storeElem x a p
    storeElem y (at a w) q
  _ -> throwError ("internal error: storing a list element of the wrong shape: " ++ show v)
  where
    store b e = emit ("sw(" ++ b ++ ", " ++ e ++ ");")
    real b r = store b ("rw0(" ++ r ++ ")") >> store (at b 1) ("rw1(" ++ r ++ ")")

-- | A variable holding zero (false, 0, black, the empty list).
zeroOf :: TypeProxy t -> Gen Loc
zeroOf t = case t of
  BooleanType -> var LBool "bool" "false"
  IntegerType -> var LInt "i32" "0"
  RealType -> var LReal "R" "ri(0)"
  ComplexType -> var LComplex "C" "C(ri(0), ri(0))"
  ColorType -> var LColor "vec3u" "vec3u(0u)"
  ListType _ -> var LList "u32" "0u"
  PairType a b -> LPair <$> zeroOf a <*> zeroOf b
  _ -> unsupported ("values of type " ++ showType t)
  where
    var wrap ty e = do
      v <- fresh
      emit ("var " ++ v ++ ": " ++ ty ++ " = " ++ e ++ ";")
      pure (wrap v)

------------------------------------------------------------------------
-- Lists

listOf :: Loc -> Gen String
listOf = \case { LList h -> pure h; l -> throwError ("internal error: a non-list: " ++ show l) }

-- | A loop over @k = 0 .. n - 1@ (@n@ an i32 expression), the body given
-- @k@'s name (an i32). The body can @break@.
forIndex :: String -> (String -> Gen ()) -> Gen ()
forIndex n body = do
  k <- fresh
  (_, b) <- capture (body k)
  nested ("for (var " ++ k ++ " = 0; " ++ k ++ " < " ++ n ++ "; " ++ k ++ "++) {") b
  emit "}"

-- | The address of element @k@ (an i32 name) of the list @h@.
slot :: String -> Int -> String -> String
slot h w k = "(" ++ h ++ " + 1u + u32(" ++ k ++ ") * " ++ show w ++ "u)"

-- | Run the body on each element's address (the body can @break@).
walkList :: TypeProxy t -> String -> (String -> Gen ()) -> Gen ()
walkList t h body = do
  w <- stride t
  n <- letName ("llen(" ++ h ++ ")")
  forIndex n (body . slot h w)

-- | A new list with room for @n@ elements (an i32 expression), which
-- @fill@ (given its address) writes, if @n > 0@ and it fits; else the
-- empty list.
newList :: TypeProxy t -> String -> (String -> Gen ()) -> Gen String
newList t n fill = do
  w <- stride t
  h <- fresh
  emit ("var " ++ h ++ ": u32 = 0u;")
  p <- fresh
  (_, filled) <- capture (fill p)
  nested ("if (" ++ n ++ " > 0) {")
    ([ "let " ++ p ++ " = alloc(1u + u32(" ++ n ++ ") * " ++ show w ++ "u);"
     , "if (" ++ p ++ " != 0u) {"
     , "  sw(" ++ p ++ ", u32(" ++ n ++ "));" ]
     ++ map ("  " ++) filled
     ++ [ "  " ++ h ++ " = " ++ p ++ ";", "}" ])
  emit "}"
  pure h

-- | Copy an element's words.
copyElem :: Int -> String -> String -> Gen ()
copyElem w src dst = forM_ [0 .. w - 1] $ \i ->
  emit ("sw(" ++ at dst i ++ ", lw(" ++ at src i ++ "));")

-- | @xs <- append(xs, x)@ (or of several elements), with @h@ the variable
-- holding xs: in place, if xs's block is in the arena, unshared (no other
-- variable or list holds it: see @lshare@), and the last thing made there,
-- so it can grow there; else a new list, as 'listJoin' makes it (which is
-- unshared, and the last thing made: the appends after it are in place).
-- So a list made by appending in a loop takes room for its elements, not
-- (as copies would) for every list it was on the way.
listAppend :: TypeProxy t -> String -> [Loc] -> Gen ()
listAppend t h es = do
  w <- stride t
  let k = length es
  (_, inPlace) <- capture $ do
    len <- letName ("llen(" ++ h ++ ")")
    p <- letName ("alloc(" ++ show (k * w) ++ "u)")
    (_, stores) <- capture (forM_ (zip [0 ..] es) $ \(i, e) -> storeElem t (at p (i * w)) e)
    nested ("if (" ++ p ++ " != 0u) {") (stores ++ ["sw(" ++ h ++ ", u32(" ++ len ++ " + " ++ show k ++ "));"])
    emit "}"
  (_, copy) <- capture $ do
    new <- listLiteral t es
    joined <- listJoin t [LList h, new]
    copyInto joined (LList h)
  nested ("if (lgrowable(" ++ h ++ ", " ++ show w ++ "u)) {") inPlace
  nested "} else {" copy
  emit "}"

listLiteral :: TypeProxy t -> [Loc] -> Gen Loc
listLiteral t elems
  | null elems = letOf LList "0u"
  | otherwise = do
      w <- stride t
      let n = length elems
      h <- newList t (show n) $ \p ->
        forM_ (zip [0 ..] elems) $ \(k, e) -> storeElem t (at p (1 + k * w)) e
      pure (LList h)

listJoin :: TypeProxy t -> [Loc] -> Gen Loc
listJoin t lists = do
  w <- stride t
  hs <- mapM listOf lists
  total <- letName (if null hs then "0" else intercalate " + " [ "llen(" ++ h ++ ")" | h <- hs ])
  h <- newList t total $ \p -> do
    k <- fresh
    emit ("var " ++ k ++ " = 0;")
    forM_ hs $ \src -> walkList t src $ \e -> do
      copyElem w e (slot p w k)
      emit (k ++ " += 1;")
  pure (LList h)

-- | The elements for which the test is false (@remove@).
listFilter :: TypeProxy t -> Loc -> (Loc -> Gen Loc) -> Gen Loc
listFilter t xs test = do
  w <- stride t
  src <- listOf xs
  n <- letName ("llen(" ++ src ++ ")")
  kept <- fresh
  emit ("var " ++ kept ++ " = 0;")
  h <- newList t n $ \p -> do
    walkList t src $ \e -> do
      v <- loadElem t e
      r <- test v >>= boolOf
      (_, keep) <- capture (copyElem w e (slot p w kept) >> emit (kept ++ " += 1;"))
      nested ("if (!" ++ r ++ ") {") keep
      emit "}"
    emit ("sw(" ++ p ++ ", u32(" ++ kept ++ "));")
  emit ("if (" ++ kept ++ " == 0) { " ++ h ++ " = 0u; }")
  pure (LList h)

listMap :: TypeProxy t1 -> TypeProxy t2 -> Loc -> (Loc -> Gen Loc) -> Gen Loc
listMap t1 t2 xs f = do
  w1 <- stride t1
  w2 <- stride t2
  src <- listOf xs
  n <- letName ("llen(" ++ src ++ ")")
  h <- newList t2 n $ \p -> forIndex n $ \k -> do
    v <- loadElem t1 (slot src w1 k)
    r <- f v
    storeElem t2 (slot p w2 k) r
  pure (LList h)

-- | The first element for which the test is true, or the default.
listFind :: TypeProxy t -> Loc -> Loc -> (Loc -> Gen Loc) -> Gen Loc
listFind t xs dflt test = do
  src <- listOf xs
  result <- varLike dflt
  copyInto dflt result
  walkList t src $ \e -> do
    v <- loadElem t e
    r <- test v >>= boolOf
    (_, take') <- capture (copyInto v result)
    nested ("if (" ++ r ++ ") {") (take' ++ ["break;"])
    emit "}"
  pure result

listLength :: Loc -> Gen Loc
listLength xs = listOf xs >>= \h -> letOf LInt ("llen(" ++ h ++ ")")

-- | 1-based: from the front for i >= 1, from the back for i <= 0, or
-- (cyclic) wrapping around.
listIndex :: TypeProxy t -> Bool -> Loc -> Loc -> Gen Loc
listIndex t cyclic xs ix = do
  w <- stride t
  h <- listOf xs
  i <- case ix of { LInt i -> pure i; l -> throwError ("internal error: an index of the wrong shape: " ++ show l) }
  len <- letName ("llen(" ++ h ++ ")")
  step <- letName $ if cyclic
    then "select(((" ++ i ++ " - 1) % " ++ len ++ " + " ++ len ++ ") % " ++ len ++ ", 0, " ++ len ++ " == 0)"
    else "select(" ++ i ++ " - 1, " ++ len ++ " + " ++ i ++ ", " ++ i ++ " <= 0)"
  ok <- letName $ if cyclic then len ++ " > 0" else "u32(" ++ step ++ ") < u32(" ++ len ++ ")"
  result <- zeroOf t
  (_, found) <- capture (loadElem t (slot h w step) >>= (`copyInto` result))
  nested ("if (" ++ ok ++ ") {") found
  emit "} else { badIndex = true; }"
  pure result

listRange :: Loc -> Loc -> Gen Loc
listRange lo hi = case (lo, hi) of
  (LInt a, LInt z) -> do
    n <- letName (z ++ " - " ++ a ++ " + 1")
    h <- newList IntegerType n $ \p -> forIndex n $ \k ->
      emit ("sw(" ++ slot p 1 k ++ ", bitcast<u32>(" ++ a ++ " + " ++ k ++ "));")
    pure (LList h)
  _ -> throwError ("internal error: a range of non-integers: " ++ show lo)

boolOf :: Loc -> Gen String
boolOf = \case { LBool b -> pure b; l -> throwError ("internal error: a test that isn't true or false: " ++ show l) }

------------------------------------------------------------------------
-- Tables

-- | A word of a table's descriptor.
desc :: String -> Int -> String
desc d k = "args[" ++ at d k ++ "]"

descReal :: String -> Int -> String
descReal d k = "rbits(" ++ desc d k ++ ", " ++ desc d (k + 1) ++ ")"

tableBytes :: TypeProxy t -> Gen Int
tableBytes t = maybe (unsupported ("tables of " ++ showType t)) pure (elementSize t)

-- | A table's element at this byte address in @tables@.
loadTableElem :: TypeProxy t -> String -> Gen Loc
loadTableElem t b = case t of
  BooleanType -> letOf LBool ("tbyte(" ++ b ++ ") != 0u")
  IntegerType -> letOf LInt ("bitcast<i32>(tables[" ++ b ++ " >> 2u])")
  RealType -> letOf LReal (real b)
  ComplexType -> letOf LComplex ("C(" ++ real b ++ ", " ++ real (at b 8) ++ ")")
  ColorType -> letOf LColor ("vec3u(tbyte(" ++ b ++ "), tbyte(" ++ at b 1 ++ "), tbyte(" ++ at b 2 ++ "))")
  PairType x y -> do
    off <- maybe (unsupported ("tables of " ++ showType t)) pure (secondOffset x)
    LPair <$> loadTableElem x b <*> loadTableElem y (at b off)
  _ -> unsupported ("tables of " ++ showType t)
  where real a = "rf64(tables[" ++ a ++ " >> 2u], tables[(" ++ a ++ " >> 2u) + 1u])"

-- | A slice's view of a table (its indices, one per dimension, an i32
-- name or a @*@): whether the integers are in range, its first cell
-- (counting from 0), and for each @*@ its count and step (i32 names).
sliceView :: String -> [Maybe String] -> Gen (String, String, [(String, String)])
sliceView d ixs = do
  inRange <- fresh
  base <- fresh
  str <- fresh
  emit ("var " ++ inRange ++ " = true;")
  emit ("var " ++ base ++ " = 0;")
  emit ("var " ++ str ++ " = 1;")
  spans <- fmap catMaybes . forM (zip [0 ..] ixs) $ \(k, mi) -> do
    dim <- letName ("i32(" ++ desc d (10 + k) ++ ")")
    r <- case mi of
      Nothing -> do
        step <- letName str
        pure (Just (dim, step))
      Just i -> do
        emit (inRange ++ " = " ++ inRange ++ " && " ++ i ++ " >= 1 && " ++ i ++ " <= " ++ dim ++ ";")
        emit (base ++ " += (" ++ i ++ " - 1) * " ++ str ++ ";")
        pure Nothing
    emit (str ++ " *= " ++ dim ++ ";")
    pure r
  pure (inRange, base, spans)

-- | The byte address of a cell (counting from 0: an i32 name).
cellByte :: TypeProxy t -> String -> String -> Gen String
cellByte t d cell = do
  size <- tableBytes t
  letName (desc d 0 ++ " * 4u + u32(" ++ cell ++ ") * " ++ show size ++ "u")

tableOf :: Loc -> Gen String
tableOf = \case { LTable d -> pure d; l -> throwError ("internal error: a non-table: " ++ show l) }

-- | A cell, by its 1-based indices.
tableRead :: TypeProxy t -> Loc -> [Loc] -> Gen Loc
tableRead t tbl ixs = do
  d <- tableOf tbl
  is <- mapM intOf ixs
  readAt t d is

readAt :: TypeProxy t -> String -> [String] -> Gen Loc
readAt t d is = do
  (inRange, base, _) <- sliceView d (map Just is)
  result <- zeroOf t
  (_, found) <- capture (cellByte t d base >>= loadTableElem t >>= (`copyInto` result))
  nested ("if (" ++ inRange ++ ") {") found
  emit "} else { badIndex = true; }"
  pure result

intOf :: Loc -> Gen String
intOf = \case { LInt i -> pure i; l -> throwError ("internal error: an index of the wrong shape: " ++ show l) }

complexOf :: Loc -> Gen String
complexOf = \case { LComplex c -> pure c; l -> throwError ("internal error: a point of the wrong shape: " ++ show l) }

-- | The column and row of the cell nearest a point (as
-- 'Language.Table.nearestCell').
pointIndices :: String -> String -> Gen (String, String)
pointIndices d z = do
  let nearest u = "rtoi(rfloor(radd(" ++ u ++ ", " ++ realLiteral 0.500001 ++ "))) + 1"
  j <- letName (nearest ("rdiv(rsub(" ++ z ++ ".re, " ++ descReal d 2 ++ "), " ++ descReal d 6 ++ ")"))
  k <- letName (nearest ("rdiv(rsub(" ++ descReal d 4 ++ ", " ++ z ++ ".im), " ++ descReal d 8 ++ ")"))
  pure (j, k)

-- | The cell nearest a point, at some indices after the first two.
tablePoint :: TypeProxy t -> Loc -> Loc -> [Loc] -> Gen Loc
tablePoint t tbl pt rest = do
  d <- tableOf tbl
  z <- complexOf pt
  (j, k) <- pointIndices d z
  is <- mapM intOf rest
  readAt t d ([j, k] ++ is)

-- | A slice with one @*@, as a list (its first two indices maybe from a
-- point); out of range, the empty list.
tableSliceRead :: TypeProxy t -> Loc -> Maybe Loc -> [Maybe Loc] -> Gen Loc
tableSliceRead t tbl mz ixs = do
  d <- tableOf tbl
  pointed <- forM mz $ \pt -> complexOf pt >>= pointIndices d <&> \(j, k) -> [Just j, Just k]
  is <- mapM (traverse intOf) ixs
  (inRange, base, spans) <- sliceView d (fromMaybe [] pointed ++ is)
  (count, step) <- case spans of
    [s] -> pure s
    _ -> throwError "internal error: a slice read as a list has one `*`"
  w <- stride t
  out <- fresh
  emit ("var " ++ out ++ ": u32 = 0u;")
  (_, readIt) <- capture $ do
    h <- newList t count $ \p -> forIndex count $ \i -> do
      cell <- letName (base ++ " + " ++ i ++ " * " ++ step)
      e <- cellByte t d cell >>= loadTableElem t
      storeElem t (slot p w i) e
    emit (out ++ " = " ++ h ++ ";")
  nested ("if (" ++ inRange ++ ") {") readIt
  emit "} else { badIndex = true; }"
  pure (LList out)

tableSize :: TypeProxy sz -> Loc -> Gen Loc
tableSize sz0 tbl = tableOf tbl >>= \d -> go d sz0 0
  where
    go :: String -> TypeProxy s -> Int -> Gen Loc
    go d sz k = case sz of
      IntegerType -> letOf LInt ("i32(" ++ desc d (10 + k) ++ ")")
      PairType IntegerType rest -> LPair <$> letOf LInt ("i32(" ++ desc d (10 + k) ++ ")") <*> go d rest (k + 1)
      _ -> throwError "internal error: a table's size doesn't fit its type"

tableContains :: Loc -> Loc -> Gen Loc
tableContains tbl pt = do
  d <- tableOf tbl
  z <- complexOf pt
  (j, k) <- pointIndices d z
  (inRange, _, _) <- sliceView d [Just j, Just k]
  letOf LBool inRange

tableCellIndices :: TypeProxy sz -> Loc -> Loc -> Gen Loc
tableCellIndices sz tbl pt = do
  d <- tableOf tbl
  z <- complexOf pt
  (j, k) <- pointIndices d z
  case sz of
    PairType IntegerType IntegerType -> pure (LPair (LInt j) (LInt k))
    _ -> throwError "internal error: a table's cell doesn't fit its type"

tableCellPoint :: Loc -> Loc -> Loc -> Gen Loc
tableCellPoint tbl mj mk = do
  d <- tableOf tbl
  j <- intOf mj
  k <- intOf mk
  letOf LComplex ("C(radd(" ++ descReal d 2 ++ ", rmul(ri(" ++ j ++ " - 1), " ++ descReal d 6 ++ ")), "
                  ++ "rsub(" ++ descReal d 4 ++ ", rmul(ri(" ++ k ++ " - 1), " ++ descReal d 8 ++ ")))")
