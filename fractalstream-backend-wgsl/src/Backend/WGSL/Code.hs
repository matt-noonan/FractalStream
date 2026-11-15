-- | Compiling a viewer's code into a WGSL compute shader (GPU_PLAN.md,
-- "Code generation"), case by case, as "Backend.Wasm.Code" and
-- "Backend.Wasm.Value" compile it to WebAssembly, so that the backends
-- compute alike (to within the GPU's precision).
--
-- The shader's entry point, @main@, runs once a pixel of a block (in
-- workgroups of 8 x 8), with the bindings "Backend.WGSL.Runtime"
-- declares: it computes its pixel's point as the CPU's kernels do (the
-- image's first pixel's point plus the pixel's index in the image times
-- the step), runs the viewer's code once a subsample, starting each from
-- grey, and writes the average color.
--
-- The configuration's values (and what the viewer's hoisted prefix
-- computed) are read from @args@, laid out by 'argWords', in the
-- environment's order. A variable of a type the shader can't have (a
-- list, a text, a table) takes no room, and code that uses it is
-- declined, as is code the generator can't compile yet ('unsupported').
module Backend.WGSL.Code
  ( kernelSource
  , argWords
  , packArgs
  , blockParams
  ) where

import FractalStream.Prelude

import Backend.WGSL.Gen
import Backend.WGSL.Runtime
import Backend.WGSL.Data

import Actor.Viewer.Types (ViewerEnv, MissingViewerArgs)
import Language.Code
import Data.Indexed.Functor
import Data.Color (colorToRGB, grey)
import Language.Table (Table(..), elementSize)
import Language.Value.Evaluator (HaskellValue)

import GHC.Float (castFloatToWord32, double2Float, float2Double)
import Numeric (showHex)

------------------------------------------------------------------------
-- The kernel

-- | A viewer's code as a WGSL module, or why it can't be one.
kernelSource :: forall env. (MissingViewerArgs env, KnownEnvironment env)
              => Precision -> Code (ViewerEnv env) -> Either String String
kernelSource p body = do
  (_, mainLines) <- runGen p $ do
    case envProxy (Proxy @(ViewerEnv env)) of
      BindingProxy xn xt (BindingProxy yn yt (BindingProxy dxn dxt (BindingProxy dyn dyt (BindingProxy cn ct rest)))) -> do
        -- the configuration's values, once
        restCtx <- evalStateT (envToContextM @GLoc rest (\_ t -> loadArg t)) 0
        let real v = do { emit ("var " ++ v ++ ": R;"); pure (LReal v) }
        x <- real "px_x"
        y <- real "px_y"
        dx <- real "px_dx"
        dy <- real "px_dy"
        emit "var px_color: vec3u;"
        let ctx = Bind xn xt x (Bind yn yt y (Bind dxn dxt dx (Bind dyn dyt dy (Bind cn ct (LColor "px_color") restCtx))))
        (_, bodyLines) <- capture (runReaderT (code body) ctx)
        let (gr, gg, gb) = colorToRGB grey
        emit "let px_x0 = radd(rp(params.x0), rmul(ri(params.offX + i32(gid.x)), rp(params.dx)));"
        emit "let px_y0 = rsub(rp(params.y0), rmul(ri(params.offY + i32(gid.y)), rp(params.dy)));"
        emit "var px_sum = vec3u(0u);"
        emit "var px_issues = 0u;"
        -- (each subsample from grey, with a fresh arena; magenta if it ran
        -- out of room or indexed out of range, as on the CPU)
        nested "for (var px_k = 0u; px_k < params.subsamples; px_k++) {"
          ([ "px_x = px_x0;", "px_y = px_y0;"
           , "px_dx = rp(params.px);", "px_dy = rp(params.py);"
           , "px_color = vec3u(" ++ show gr ++ "u, " ++ show gg ++ "u, " ++ show gb ++ "u);"
           , "bump = 1u;", "overflow = false;", "badIndex = false;" ]
           ++ bodyLines
           ++ [ "if (overflow || badIndex) { px_color = vec3u(255u, 0u, 255u); }"
              , "px_issues |= select(0u, 1u, overflow) | select(0u, 2u, badIndex);"
              , "px_sum += px_color;" ])
        emit "}"
        emit "let px_c = px_sum / max(params.subsamples, 1u);"
        emit "out[gid.y * params.width + gid.x] = cpack(px_c) | (px_issues << 24u);"
  pure $ unlines $
    [ runtime p
    , "@compute @workgroup_size(8, 8)"
    , "fn main(@builtin(global_invocation_id) gid: vec3u) {"
    , "  if (gid.x >= params.width || gid.y >= params.height) { return; }" ]
    ++ map ("  " ++) mainLines
    ++ [ "}" ]

-- | Read a variable from the arguments, at this word ('argWords' lays
-- them out).
loadArg :: TypeProxy t -> StateT Int Gen Loc
loadArg t = do
  off <- get
  let w k = "args[" ++ show (off + k) ++ "]"
      real k = "rbits(" ++ w k ++ ", " ++ w (k + 1) ++ ")"
      var :: (String -> Loc) -> String -> String -> Int -> StateT Int Gen Loc
      var wrap ty e n = do
        v <- lift fresh
        lift (emit ("var " ++ v ++ ": " ++ ty ++ " = " ++ e ++ ";"))
        put (off + n)
        pure (wrap v)
  case t of
    BooleanType -> var LBool "bool" (w 0 ++ " != 0u") 1
    IntegerType -> var LInt "i32" ("bitcast<i32>(" ++ w 0 ++ ")") 1
    RealType -> var LReal "R" (real 0) 2
    ComplexType -> var LComplex "C" ("C(" ++ real 0 ++ ", " ++ real 2 ++ ")") 4
    ColorType -> var LColor "vec3u" ("vec3u(" ++ w 0 ++ " & 255u, (" ++ w 0 ++ " >> 8u) & 255u, (" ++ w 0 ++ " >> 16u) & 255u)") 1
    PairType a b -> LPair <$> loadArg a <*> loadArg b
    ListType i | Just _ <- elemWords i -> var LList "u32" (w 0) 1
    TableType _ i | Just _ <- elementSize i -> var LTable "u32" (w 0) 1
    _ -> pure (LNone (typeName t))

-- | A value as the words 'loadArg' reads (none for a type it can't
-- have): a truth value or integer one (integers are 32 bits, as in the
-- CPU's compiled code), a real number two (hi and lo: their sum is the
-- double, to about 48 bits), a complex number four, a color one
-- (@0x00BBGGRR@), and a pair its parts'. (A list or table is an address:
-- see 'packArgs'.)
argWords :: TypeProxy t -> HaskellType t -> [Word32]
argWords t v = evalState (slotOf t v) (Packed [] 0 [] 0)

-- | The arguments buffer's contents: the configuration's values, each in
-- its slot as 'loadArg' reads it, then the blocks of its lists and the
-- descriptors of its tables (see "Backend.WGSL.Runtime"); and its tables,
-- each with where its elements start in @tables@ (in bytes, a multiple
-- of 8) and how many bytes they take, in that order.
packArgs :: Context HaskellValue env -> ([Word32], [(Table, Int, Int)])
packArgs ctx =
  let values = fromContext (\_ t v -> SomeArg t v) ctx
      slots = sum [ slotSize t | SomeArg t _ <- values ]
      (ws, st) = runState (concat <$> mapM (\(SomeArg t v) -> slotOf t v) values) (Packed [] slots [] 0)
  in (ws ++ concat (reverse (pkHeap st)), reverse (pkTables st))

data SomeArg where
  SomeArg :: TypeProxy t -> HaskellType t -> SomeArg

data Packed = Packed
  { pkHeap :: [[Word32]]       -- ^ the blocks after the slots, newest first
  , pkNext :: Int              -- ^ the next block's word
  , pkTables :: [(Table, Int, Int)] -- ^ newest first
  , pkTableBytes :: Int        -- ^ where the next table's elements go
  }

slotSize :: TypeProxy t -> Int
slotSize = \case
  BooleanType -> 1
  IntegerType -> 1
  RealType -> 2
  ComplexType -> 4
  ColorType -> 1
  PairType a b -> slotSize a + slotSize b
  ListType i | Just _ <- elemWords i -> 1
  TableType _ i | Just _ <- elementSize i -> 1
  _ -> 0

-- | A block after the slots: its word.
block :: [Word32] -> State Packed Int
block ws = do
  st <- get
  put st { pkHeap = ws : pkHeap st, pkNext = pkNext st + length ws }
  pure (pkNext st)

slotOf :: TypeProxy t -> HaskellType t -> State Packed [Word32]
slotOf t v = case t of
  BooleanType -> pure [if v then 1 else 0]
  IntegerType -> pure [fromIntegral (fromIntegral v :: Int32)]
  RealType -> pure (realWords v)
  ComplexType -> let a :+ b = v in pure (realWords a ++ realWords b)
  ColorType -> let (r, g, b) = colorToRGB v
               in pure [fromIntegral r + 256 * fromIntegral g + 65536 * fromIntegral b]
  PairType a b -> (++) <$> slotOf a (fst v) <*> slotOf b (snd v)
  -- (a list's elements are as its slots would be; in args, its address
  -- has the top bit set)
  ListType i | Just _ <- elemWords i -> case v of
    [] -> pure [0]
    _ -> do
      elems <- concat <$> mapM (slotOf i) v
      a <- block (fromIntegral (length v) : elems)
      pure [0x80000000 + fromIntegral a]
  TableType _ i | Just size <- elementSize i -> do
    st <- get
    off <- case lookup (tableVersion v) [ (tableVersion t', o) | (t', o, _) <- pkTables st ] of
      Just o -> pure o
      Nothing -> do
        let o = pkTableBytes st
            bytes = size * product (tableDims v)
        put st { pkTables = (v, o, bytes) : pkTables st, pkTableBytes = o + (bytes + 7) `div` 8 * 8 }
        pure o
    let (ox, oy) = tableOrigin v
        (sx, sy) = tableStep v
    d <- block ([ fromIntegral (off `div` 4), fromIntegral (length (tableDims v)) ]
                ++ concatMap realWords [ox, oy, sx, sy] ++ map fromIntegral (tableDims v))
    pure [fromIntegral d]
  _ -> pure []

realWords :: Double -> [Word32]
realWords x = let (hi, lo) = split x in [castFloatToWord32 hi, castFloatToWord32 lo]

-- | A double as two f32s, the second the rest.
split :: Double -> (Float, Float)
split x
  | isNaN x || isInfinite x = (double2Float x, 0)
  | otherwise = let hi = double2Float x in (hi, double2Float (x - float2Double hi))

-- | A block's parameters ('Backend.WGSL.Device.paramsSize' bytes, as
-- "Backend.WGSL.Runtime"'s @Params@): its size, how many subsamples, where
-- it is in the image, the image's first pixel's point, the step, and the
-- pixel size the code sees, as words.
blockParams :: (Int32, Int32) -> Int32 -> (Int32, Int32) -> (Double, Double) -> (Double, Double) -> (Double, Double)
            -> [Word32]
blockParams (w, h) subsamples (ox, oy) (x0, y0) (dx, dy) (px, py) =
  [ fromIntegral w, fromIntegral h, fromIntegral subsamples, 0
  , fromIntegral ox, fromIntegral oy, 0, 0 ]
  ++ concatMap realWords [x0, y0, dx, dy, px, py]

-- | Whether a value of this type holds a list.
holdsList :: TypeProxy t -> Bool
holdsList = \case
  ListType _ -> True
  PairType a b -> holdsList a || holdsList b
  _ -> False

typeName :: TypeProxy t -> String
typeName = \case
  ListType _ -> "lists"
  TextType -> "text"
  TableType _ _ -> "tables"
  t -> "values of type " ++ showType t

------------------------------------------------------------------------
-- Code

data CtxCode :: Environment -> Exp Type
type instance Eval (CtxCode env) = ReaderT (Context GLoc env) Gen ()

code :: Code env -> ReaderT (Context GLoc env) Gen ()
code = indexedFold @CtxCode $ \case
  Block body -> sequence_ body

  NoOp -> pure ()

  Set pf _ e -> do
    x <- value e
    ctx <- ask
    when (holdsList (typeOfValue e)) (lift noteListSet)
    lift (copyInto x (withKnownType (typeOfValue e) (getBinding ctx pf)))

  Let pf name val body -> do
    x <- value val
    x' <- lift (varLike x)
    lift (copyInto x x')
    recallIsAbsent (absentInTail pf) $ do
      ctx <- Bind name (typeOfValue val) x' <$> ask
      lift (runReaderT body ctx)

  IfThenElse cond yes no -> value cond >>= \case
    LBool c -> do
      (_, yesLines) <- captured yes
      (_, noLines) <- captured no
      lift $ do
        nested ("if (" ++ c ++ ") {") yesLines
        nested "} else {" noLines
        emit "}"
    l -> lift (mismatch l)

  -- (the body, then the test: again while it's true. Its lists go once
  -- the test is done, if none can outlive the iteration: see 'reclaiming')
  DoWhile cond body -> do
    ctx <- ask
    (_, loopLines) <- lift $ reclaiming $ \giveBack -> flip runReaderT ctx $ do
      body
      value cond >>= \case
        LBool c -> lift (giveBack >> emit ("if (!" ++ c ++ ") { break; }"))
        l -> lift (mismatch l)
    lift (nested "loop {" loopLines >> emit "}")

  ForEach pfList _ (ListType itemTy) itemName pfNoItem _ _ body -> recallIsAbsent pfNoItem $ do
    ctx <- ask
    case withKnownType (ListType itemTy) (getBinding ctx pfList) of
      LList h -> lift $ walkList itemTy h $ \a -> do
        (_, ls) <- reclaiming $ \giveBack -> do
          e <- loadElem itemTy a
          runReaderT body (Bind itemName itemTy e ctx)
          giveBack
        mapM_ emit ls
      l -> lift (mismatch l)

  -- the first element for which the predicate holds: the action with it,
  -- or the fallback (if any) when there's none
  Lookup pfList _ (ListType itemTy) itemName pfNoItem _ _ predicate action fallback ->
    recallIsAbsent pfNoItem $ do
    ctx <- ask
    case withKnownType (ListType itemTy) (getBinding ctx pfList) of
      LList h -> lift $ do
        found <- fresh
        emit ("var " ++ found ++ " = false;")
        item <- zeroOf itemTy
        walkList itemTy h $ \a -> do
          e <- loadElem itemTy a
          runReaderT (value predicate) (Bind itemName itemTy e ctx) >>= \case
            LBool b -> do
              (_, take') <- capture (copyInto e item)
              nested ("if (" ++ b ++ ") {") (take' ++ [found ++ " = true;", "break;"])
              emit "}"
            l -> mismatch l
        (_, yes) <- capture (runReaderT action (Bind itemName itemTy item ctx))
        (_, no) <- capture (maybe (pure ()) (`runReaderT` ctx) fallback)
        nested ("if (" ++ found ++ ") {") yes
        nested "} else {" no
        emit "}"
      l -> lift (mismatch l)
  DrawCommand _ -> lift (unsupported "drawing")
  TableAllocate {} -> lift (unsupported "tables")
  TableWrite {} -> lift (unsupported "tables")
  TableWriteSlice {} -> lift (unsupported "tables")
  TableCopySlice {} -> lift (unsupported "tables")
  TableResize {} -> lift (unsupported "tables")
  where
    captured :: ReaderT r Gen a -> ReaderT r Gen (a, [String])
    captured m = ReaderT (capture . runReaderT m)

------------------------------------------------------------------------
-- Values

data CtxLoc :: (Environment, FSType) -> Exp Type
type instance Eval (CtxLoc et) = ReaderT (Context GLoc (Env et)) Gen Loc

value :: forall env t. Value '(env, t) -> ReaderT (Context GLoc env) Gen Loc
value = indexedFold go'
  where
    go' :: forall et. ValueF CtxLoc et -> Eval (CtxLoc et)
    go' x = case toIndex x of { EnvType _ -> go x }

    go :: forall env' t'. KnownEnvironment env'
       => ValueF CtxLoc '(env', t') -> ReaderT (Context GLoc env') Gen Loc
    go = \case
      -- (a copy, so that setting the variable later can't change a value
      -- computed from it)
      Var _ t pf -> withKnownType t $ do
        ctx <- ask
        lift (copied (getBinding ctx pf))

      LocalLet name vt pf v _ e -> recallIsAbsent pf $ do
        x <- v
        x' <- lift (copied x)
        withReaderT (Bind name vt x') e

      Const (Scalar t c) -> lift (constant t c)

      PairV _ x y -> LPair <$> x <*> y
      ProjV1 _ p -> p >>= \case { LPair a _ -> pure a; l -> lift (mismatch l) }
      ProjV2 _ p -> p >>= \case { LPair _ b -> pure b; l -> lift (mismatch l) }

      R2C r -> r >>= lift . force >>= \case
        LReal x -> lift (letOf LComplex ("C(" ++ x ++ ", ri(0))"))
        l -> lift (mismatch l)
      I2R i -> i >>= \case
        LInt x -> lift (letOf LReal ("ri(" ++ x ++ ")"))
        l -> lift (mismatch l)
      C2R2 z -> z >>= \case
        LComplex c -> lift (LPair <$> letOf LReal (c ++ ".re") <*> letOf LReal (c ++ ".im"))
        l -> lift (mismatch l)
      ReC z -> z >>= \case { LComplex c -> lift (letOf LReal (c ++ ".re")); l -> lift (mismatch l) }
      ImC z -> z >>= \case { LComplex c -> lift (letOf LReal (c ++ ".im")); l -> lift (mismatch l) }

      -- (made short-circuiting before: see Language.Value.Transform)
      And x y -> bool2 "&&" x y
      Or x y -> bool2 "||" x y
      Not b -> b >>= \case
        LBool x -> lift (letOf LBool ("!" ++ x))
        l -> lift (mismatch l)

      -- Only the branch taken is computed.
      ITE _ mc myes mno -> mc >>= \case
        LBool c -> do
          (yes, yesLines) <- captureR myes
          (no, noLines) <- captureR mno
          lift $ do
            result <- varLike yes
            (_, yesCopy) <- capture (copyInto yes result)
            (_, noCopy) <- capture (copyInto no result)
            nested ("if (" ++ c ++ ") {") (yesLines ++ yesCopy)
            nested "} else {" (noLines ++ noCopy)
            emit "}"
            pure result
        l -> lift (mismatch l)

      RGB mr mg mb -> do
        r <- mr >>= lift . realOf
        g <- mg >>= lift . realOf
        b <- mb >>= lift . realOf
        lift (letOf LColor ("vec3u(cbyte(" ++ r ++ "), cbyte(" ++ g ++ "), cbyte(" ++ b ++ "))"))
      InvertRGB c -> c >>= \case
        LColor k -> lift (letOf LColor ("vec3u(255u) - " ++ k))
        l -> lift (mismatch l)
      Blend ms mc mc' -> do
        s <- ms >>= lift . realOf
        c <- mc >>= lift . colorOf
        c' <- mc' >>= lift . colorOf
        lift (letOf LColor ("cblend(" ++ s ++ ", " ++ c ++ ", " ++ c' ++ ")"))

      Eql t x y -> do { a <- x; b <- y; lift (equal t a b) }
      NEq t x y -> do { a <- x; b <- y; lift (notEqual t a b) }
      LTI x y -> int2 LBool (infixOp "<") x y
      -- (|z| < r, or r < |z|, without overflowing: squared where that's
      -- safe, as the CPU does, else scaled: see Backend.WGSL.Runtime)
      LTF x y -> do
        a <- x; b <- y
        case (a, b) of
          (LAbsC z, _) -> lift (realOf b >>= \r -> letOf LBool (call "cabslt" z r))
          (_, LAbsC z) -> lift (realOf a >>= \l -> letOf LBool (call "cabsgt" z l))
          _ -> real2 LBool (call "rlt") (pure a) (pure b)

      AddI x y -> int2 LInt (infixOp "+") x y
      SubI x y -> int2 LInt (infixOp "-") x y
      MulI x y -> int2 LInt (infixOp "*") x y
      DivI x y -> int2 LInt (call "idiv") x y
      ModI x y -> int2 LInt (call "imod") x y
      PowI x y -> int2 LInt (call "ipow") x y
      NegI i -> int1 i (\a -> "0 - " ++ a)
      AbsI i -> int1 i (\a -> "iabs(" ++ a ++ ")")

      AddF x y -> real2 LReal (call "radd") x y
      SubF x y -> real2 LReal (call "rsub") x y
      MulF x y -> real2 LReal (call "rmul") x y
      DivF x y -> real2 LReal (call "rdiv") x y
      ModF x y -> real2 LReal (call "rmod") x y
      PowF x y -> real2 LReal (call "rpow") x y
      NegF r -> real1 LReal "rneg" r
      AbsF r -> real1 LReal "rabs" r
      SqrtF r -> real1 LReal "rsqrt" r
      -- (`round` truncates, as the CPU's does)
      RoundF r -> real1 LInt "rtoi(rtrunc" r
      FloorF r -> real1 LInt "rtoi(rfloor" r
      CeilingF r -> real1 LInt "rtoi(rceil" r

      ExpF r -> native "exp" r
      LogF r -> native "log" r
      SinF r -> native "sin" r
      CosF r -> native "cos" r
      TanF r -> native "tan" r
      SinhF r -> native "sinh" r
      CoshF r -> native "cosh" r
      TanhF r -> native "tanh" r
      ArcsinF r -> native "asin" r
      ArccosF r -> native "acos" r
      ArctanF r -> native "atan" r
      ArcsinhF r -> native "asinh" r
      ArccoshF r -> native "acosh" r
      ArctanhF r -> native "atanh" r
      Arctan2F y x -> real2 LReal (call "ratan2") y x

      AddC x y -> complex2 "cadd" x y
      SubC x y -> complex2 "csub" x y
      MulC x y -> complex2 "cmul" x y
      DivC x y -> complex2 "cdiv" x y
      NegC z -> complex1 LComplex "cneg" z
      ConjC z -> complex1 LComplex "cconj" z
      AbsC z -> z >>= \case
        LComplex c -> pure (LAbsC c)
        l -> lift (mismatch l)
      SqrtC z -> complex1 LComplex "csqrt" z
      ArgC z -> complex1 LReal "carg" z
      ExpC z -> nativeC "exp" z
      LogC z -> nativeC "log" z
      SinC z -> nativeC "sin" z
      CosC z -> nativeC "cos" z
      TanC z -> nativeC "tan" z
      SinhC z -> nativeC "sinh" z
      CoshC z -> nativeC "cosh" z
      TanhC z -> nativeC "tanh" z
      PowC x y -> complex2 "cpow" x y

      List ty mElems -> sequence mElems >>= lift . listLiteral ty
      Join ty mLists -> sequence mLists >>= lift . listJoin ty
      Remove name ty pf mxs mtest -> recallIsAbsent pf $ do
        ctx <- ask
        xs <- mxs
        lift (listFilter ty xs (\e -> runReaderT mtest (Bind name ty e ctx)))
      Transform name ty1 ty2 pf mxs mf -> recallIsAbsent pf $ do
        ctx <- ask
        xs <- mxs
        lift (listMap ty1 ty2 xs (\e -> runReaderT mf (Bind name ty1 e ctx)))
      Find name ty pf mxs mtest mdefault -> recallIsAbsent pf $ do
        ctx <- ask
        xs <- mxs
        dflt <- mdefault
        lift (listFind ty xs dflt (\e -> runReaderT mtest (Bind name ty e ctx)))
      Length _ mxs -> mxs >>= lift . listLength
      Index ty cyclic mxs mi -> do
        xs <- mxs
        i <- mi
        lift (listIndex ty cyclic xs i)
      Range mlo mhi -> do
        lo <- mlo
        hi <- mhi
        lift (listRange lo hi)
      ConcatText {} -> lift (unsupported "text")
      ToText {} -> lift (unsupported "text")
      Autocolor {} -> lift (unsupported "autocolor")
      TableIndex _ ty mt mixs -> do
        t <- mt
        ixs <- sequence mixs
        lift (tableRead ty t ixs)
      TablePoint _ ty mt mz mixs -> do
        t <- mt
        z <- mz
        ixs <- sequence mixs
        lift (tablePoint ty t z ixs)
      TableSlice _ ty mt mmz mixs -> do
        t <- mt
        z <- sequence mmz
        ixs <- mapM sequence mixs
        lift (tableSliceRead ty t z ixs)
      TableSize _ _ sz mt -> mt >>= lift . tableSize sz
      TableContains _ _ mt mz -> do
        t <- mt
        z <- mz
        lift (tableContains t z)
      TableCell _ _ sz mt mz -> do
        t <- mt
        z <- mz
        lift (tableCellIndices sz t z)
      TableCellPoint _ _ mt mj mk -> do
        t <- mt
        j <- mj
        k <- mk
        lift (tableCellPoint t j k)

    -- (see Backend.WGSL.Runtime: rexp, cexp, ...)
    native f r = real1 LReal ('r' : f) r
    nativeC f z = complex1 LComplex ('c' : f) z

------------------------------------------------------------------------
-- Helpers

captureR :: ReaderT r Gen a -> ReaderT r Gen (a, [String])
captureR m = ReaderT (capture . runReaderT m)

mismatch :: Loc -> Gen a
mismatch l = throwError ("internal error: a value of the wrong shape in the GPU backend: " ++ show l)

-- | A copy of a value, in fresh @let@s.
copied :: Loc -> Gen Loc
copied = force >=> \case
  LBool a -> letOf LBool a
  LInt a -> letOf LInt a
  LReal a -> letOf LReal a
  LComplex a -> letOf LComplex a
  LColor a -> letOf LColor a
  LList a -> letOf LList a
  LTable a -> letOf LTable a
  LPair a b -> LPair <$> copied a <*> copied b
  LNone what -> unsupported what
  l@(LAbsC _) -> mismatch l

realOf, colorOf :: Loc -> Gen String
realOf = force >=> \case { LReal r -> pure r; l -> mismatch l }
colorOf = \case { LColor c -> pure c; l -> mismatch l }

call :: String -> String -> String -> String
call f a b = f ++ "(" ++ a ++ ", " ++ b ++ ")"

infixOp :: String -> String -> String -> String
infixOp o a b = a ++ " " ++ o ++ " " ++ b

bool2 :: String -> ReaderT r Gen Loc -> ReaderT r Gen Loc -> ReaderT r Gen Loc
bool2 o x y = do
  a <- x; b <- y
  case (a, b) of
    (LBool p, LBool q) -> lift (letOf LBool (p ++ " " ++ o ++ " " ++ q))
    _ -> lift (mismatch a)

int1 :: ReaderT r Gen Loc -> (String -> String) -> ReaderT r Gen Loc
int1 i f = i >>= \case
  LInt a -> lift (letOf LInt (f a))
  l -> lift (mismatch l)

int2 :: (String -> Loc) -> (String -> String -> String) -> ReaderT r Gen Loc -> ReaderT r Gen Loc -> ReaderT r Gen Loc
int2 wrap f x y = do
  a <- x; b <- y
  case (a, b) of
    (LInt p, LInt q) -> lift (letOf wrap (f p q))
    _ -> lift (mismatch a)

-- | A function of one real (its name, with any opening parentheses of
-- functions around it: closed to match).
real1 :: (String -> Loc) -> String -> ReaderT r Gen Loc -> ReaderT r Gen Loc
real1 wrap f r = r >>= lift . force >>= \case
  LReal a -> lift (letOf wrap (f ++ "(" ++ a ++ ")" ++ replicate (length (filter (== '(') f)) ')'))
  l -> lift (mismatch l)

real2 :: (String -> Loc) -> (String -> String -> String) -> ReaderT r Gen Loc -> ReaderT r Gen Loc -> ReaderT r Gen Loc
real2 wrap f x y = do
  a <- x >>= lift . force
  b <- y >>= lift . force
  case (a, b) of
    (LReal p, LReal q) -> lift (letOf wrap (f p q))
    _ -> lift (mismatch a)

complex1 :: (String -> Loc) -> String -> ReaderT r Gen Loc -> ReaderT r Gen Loc
complex1 wrap f z = z >>= \case
  LComplex a -> lift (letOf wrap (f ++ "(" ++ a ++ ")"))
  l -> lift (mismatch l)

complex2 :: String -> ReaderT r Gen Loc -> ReaderT r Gen Loc -> ReaderT r Gen Loc
complex2 f x y = do
  a <- x; b <- y
  case (a, b) of
    (LComplex p, LComplex q) -> lift (letOf LComplex (call f p q))
    _ -> lift (mismatch a)

-- | A constant. A real number outside f32's range (nonzero, and finite)
-- can't be one, in either precision.
constant :: TypeProxy t -> HaskellType t -> Gen Loc
constant t c = case t of
  BooleanType -> letOf LBool (if c then "true" else "false")
  IntegerType -> letOf LInt ("i32(" ++ show (fromIntegral c :: Int32) ++ ")")
  RealType -> real c >>= letOf LReal
  ComplexType -> do
    let a :+ b = c
    ra <- real a
    rb <- real b
    letOf LComplex ("C(" ++ ra ++ ", " ++ rb ++ ")")
  ColorType -> do
    let (r, g, b) = colorToRGB c
    letOf LColor ("vec3u(" ++ show r ++ "u, " ++ show g ++ "u, " ++ show b ++ "u)")
  PairType t1 t2 -> let (x, y) = c in LPair <$> constant t1 x <*> constant t2 y
  _ -> unsupported (typeName t)
  where
    real x = do
      unless (inRange x) $
        unsupported ("numbers as large or as small as " ++ show x)
      pure $ case realWords x of
        [hi, lo] -> "rbits(0x" ++ showHex hi "u, 0x" ++ showHex lo "u)"
        _ -> "ri(0)"
    inRange x = isNaN x || isInfinite x || x == 0
                || (abs x >= 1.1754944e-38 && abs x <= 3.4028235e38)

equal :: TypeProxy t -> Loc -> Loc -> Gen Loc
equal t a0 b0 = force a0 >>= \a -> force b0 >>= \b -> case (t, a, b) of
  (BooleanType, LBool p, LBool q) -> letOf LBool (p ++ " == " ++ q)
  (IntegerType, LInt p, LInt q) -> letOf LBool (p ++ " == " ++ q)
  (RealType, LReal p, LReal q) -> letOf LBool (call "req" p q)
  (ComplexType, LComplex p, LComplex q) -> letOf LBool (call "ceq" p q)
  (ColorType, LColor p, LColor q) -> letOf LBool ("all(" ++ p ++ " == " ++ q ++ ")")
  (PairType t1 t2, LPair x1 x2, LPair y1 y2) -> do
    c1 <- equal t1 x1 y1
    c2 <- equal t2 x2 y2
    case (c1, c2) of
      (LBool p, LBool q) -> letOf LBool (p ++ " && " ++ q)
      _ -> mismatch c1
  _ -> unsupported ("comparing values of type " ++ showType t)

-- | Inequality: for real numbers, "ordered and unequal", as the CPU's
-- (NaN is unequal to nothing).
notEqual :: TypeProxy t -> Loc -> Loc -> Gen Loc
notEqual t a0 b0 = force a0 >>= \a -> force b0 >>= \b -> case (t, a, b) of
  (BooleanType, LBool p, LBool q) -> letOf LBool (p ++ " != " ++ q)
  (IntegerType, LInt p, LInt q) -> letOf LBool (p ++ " != " ++ q)
  (RealType, LReal p, LReal q) -> letOf LBool (call "rne" p q)
  (ComplexType, LComplex p, LComplex q) -> letOf LBool (call "cne" p q)
  (ColorType, LColor p, LColor q) -> letOf LBool ("any(" ++ p ++ " != " ++ q ++ ")")
  (PairType t1 t2, LPair x1 x2, LPair y1 y2) -> do
    c1 <- notEqual t1 x1 y1
    c2 <- notEqual t2 x2 y2
    case (c1, c2) of
      (LBool p, LBool q) -> letOf LBool (p ++ " || " ++ q)
      _ -> mismatch c1
  _ -> unsupported ("comparing values of type " ++ showType t)
