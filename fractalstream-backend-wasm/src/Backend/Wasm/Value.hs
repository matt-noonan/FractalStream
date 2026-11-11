{-# language OverloadedStrings #-}
-- | Compiling values to WebAssembly: case by case, as
-- "Backend.LLVM.Value" does, so that the two backends compute alike.
-- Each value's result goes into fresh locals (see "Backend.Wasm.Gen").
--
-- Not compiled yet (the caller falls back to the interpreter): text and
-- tables.
module Backend.Wasm.Value
  ( value
  ) where

import FractalStream.Prelude

import Backend.Wasm.Encode
import Backend.Wasm.Gen
import Backend.Wasm.List

import Language.Value
import Data.Indexed.Functor
import Data.Color (colorToRGB)

data CtxLoc :: (Environment, FSType) -> Exp Type
type instance Eval (CtxLoc et) = ReaderT (Context WLoc (Env et)) Gen Loc

value :: forall env t. Value '(env, t) -> ReaderT (Context WLoc env) Gen Loc
value = indexedFold go'
  where
    go' :: forall et. ValueF CtxLoc et -> Eval (CtxLoc et)
    go' x = case toIndex x of { EnvType _ -> go x }

    go :: forall env' t'. KnownEnvironment env'
       => ValueF CtxLoc '(env', t') -> ReaderT (Context WLoc env') Gen Loc
    go = \case
      -- (a copy, so that setting the variable later can't change a value
      -- computed from it: see 'copyInto')
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

      R2C r -> r >>= \case
        LReal x -> lift $ do
          y <- newLocal F64
          emit [f64Const 0, localSet y]
          pure (LComplex x y)
        l -> lift (mismatch l)
      I2R i -> i >>= \case
        LInt x -> lift (unaryTo F64 LReal [localGet x, f64ConvertI32S])
        l -> lift (mismatch l)
      C2R2 z -> z >>= \case
        LComplex x y -> pure (LPair (LReal x) (LReal y))
        l -> lift (mismatch l)
      ReC z -> z >>= \case { LComplex x _ -> pure (LReal x); l -> lift (mismatch l) }
      ImC z -> z >>= \case { LComplex _ y -> pure (LReal y); l -> lift (mismatch l) }

      -- (as in the LLVM backend, both sides are computed: short-circuiting
      -- is done before, by 'Language.Value.Transform.shortCircuit')
      And x y -> bool2 i32And x y
      Or x y -> bool2 i32Or x y
      Not b -> b >>= \case
        LBool x -> lift (unaryTo I32 LBool [localGet x, i32Eqz])
        l -> lift (mismatch l)

      -- Only the branch taken is computed: some values have side effects.
      ITE _ mc myes mno -> mc >>= \case
        LBool c -> do
          (yes, yesCode) <- captureR myes
          (no, noCode) <- captureR mno
          lift $ do
            result <- freshLike yes
            (_, yesCopy) <- capture (copyInto yes result)
            (_, noCopy) <- capture (copyInto no result)
            emit [localGet c, IfElse (yesCode ++ yesCopy) (noCode ++ noCopy)]
            pure result
        l -> lift (mismatch l)

      RGB mr mg mb -> do
        r <- mr; g <- mg; b <- mb
        lift (LColor <$> byte r <*> byte g <*> byte b)

      InvertRGB c -> c >>= \case
        LColor r g b -> lift $ do
          let inv k = unaryTo I32 id [i32Const 255, localGet k, i32Sub]
          LColor <$> inv r <*> inv g <*> inv b
        l -> lift (mismatch l)

      Blend ms mc mc' -> do
        s <- ms; c <- mc; c' <- mc'
        case (s, c, c') of
          (LReal s', LColor r g b, LColor r' g' b') -> lift $ do
            -- s * c + (1 - s) * c', truncated to a byte (as the LLVM
            -- backend's fptoui: saturating at 0, and keeping the low byte)
            let mix k k' = unaryTo I32 id
                  [ localGet s', localGet k, f64ConvertI32U, f64Mul
                  , f64Const 1, localGet s', f64Sub, localGet k', f64ConvertI32U, f64Mul
                  , f64Add, i32TruncSatF64U, i32Const 255, i32And ]
            LColor <$> mix r r' <*> mix g g' <*> mix b b'
          _ -> lift (mismatch s)

      Eql t x y -> do { a <- x; b <- y; lift (equal t a b) }
      NEq t x y -> do { a <- x; b <- y; lift (notEqual t a b) }

      LTI x y -> int2 LBool i32LtS x y
      LTF x y -> do
        a <- x; b <- y
        case (a, b) of
          (LReal p, LReal q) -> lift (unaryTo I32 LBool [localGet p, localGet q, f64Lt])
          _ -> lift (mismatch a)

      AddI x y -> int2 LInt i32Add x y
      SubI x y -> int2 LInt i32Sub x y
      MulI x y -> int2 LInt i32Mul x y
      DivI x y -> do { a <- x; b <- y; lift (intDivision True a b) }
      ModI x y -> do { a <- x; b <- y; lift (intDivision False a b) }
      NegI i -> i >>= \case
        LInt x -> lift (unaryTo I32 LInt [i32Const 0, localGet x, i32Sub])
        l -> lift (mismatch l)
      PowI mx my -> do
        a <- mx; b <- my
        case (a, b) of
          (LInt x, LInt n) -> lift (intPow x n)
          _ -> lift (mismatch a)
      -- (the absolute value of the smallest integer is itself, as with
      -- llvm.abs without poison)
      AbsI i -> i >>= \case
        LInt x -> lift (unaryTo I32 LInt
          [ i32Const 0, localGet x, i32Sub, localGet x
          , localGet x, i32Const 0, i32LtS, select ])
        l -> lift (mismatch l)

      AddF x y -> real2 f64Add x y
      SubF x y -> real2 f64Sub x y
      MulF x y -> real2 f64Mul x y
      DivF x y -> real2 f64Div x y
      -- fmod, made nonnegative for a positive divisor (as the LLVM backend)
      ModF x y -> do
        a <- x; b <- y
        case (a, b) of
          (LReal p, LReal q) -> lift $ do
            m <- newLocal F64
            emit [localGet p, localGet q, call (mathFn "fmod"), localSet m]
            unaryTo F64 LReal
              [ localGet m, localGet q, f64Add, localGet m
              , localGet m, f64Const 0, f64Lt, select ]
          _ -> lift (mismatch a)
      NegF r -> r >>= \case
        LReal x -> lift (unaryTo F64 LReal [f64Const 0, localGet x, f64Sub])
        l -> lift (mismatch l)

      AddC x y -> complex2 x y $ \(a, b) (c, d) -> do
        LComplex <$> realOf [localGet a, localGet c, f64Add]
                 <*> realOf [localGet b, localGet d, f64Add]
      SubC x y -> complex2 x y $ \(a, b) (c, d) -> do
        LComplex <$> realOf [localGet a, localGet c, f64Sub]
                 <*> realOf [localGet b, localGet d, f64Sub]
      MulC x y -> complex2 x y complexMul
      DivC x y -> complex2 x y complexDiv
      NegC z -> complex1 z $ \(a, b) ->
        LComplex <$> realOf [f64Const 0, localGet a, f64Sub]
                 <*> realOf [f64Const 0, localGet b, f64Sub]
      ConjC z -> complex1 z $ \(a, b) ->
        LComplex a <$> realOf [f64Const 0, localGet b, f64Sub]

      -- (`round` truncates, as the LLVM backend's does)
      RoundF r -> real1To I32 LInt r [f64Trunc, i32TruncSatF64S]
      FloorF r -> real1To I32 LInt r [f64Floor, i32TruncSatF64S]
      CeilingF r -> real1To I32 LInt r [f64Ceil, i32TruncSatF64S]
      AbsF r -> real1To F64 LReal r [f64Abs]
      SqrtF r -> real1To F64 LReal r [f64Sqrt]
      PowF x y -> real2 (call (mathFn "pow")) x y
      LogF r -> math1 "log" r
      ExpF r -> math1 "exp" r
      CosF r -> math1 "cos" r
      SinF r -> math1 "sin" r
      TanF r -> math1 "tan" r
      SinhF r -> math1 "sinh" r
      CoshF r -> math1 "cosh" r
      TanhF r -> math1 "tanh" r
      ArcsinF r -> math1 "asin" r
      ArccosF r -> math1 "acos" r
      ArctanF r -> math1 "atan" r
      Arctan2F y x -> real2 (call (mathFn "atan2")) y x
      ArcsinhF r -> math1 "asinh" r
      ArccoshF r -> math1 "acosh" r
      ArctanhF r -> math1 "atanh" r

      AbsC z -> complex1 z $ \(a, b) ->
        LReal <$> realOf [localGet a, localGet b, call (mathFn "hypot")]
      ArgC z -> complex1 z $ \(a, b) ->
        LReal <$> realOf [localGet b, localGet a, call (mathFn "atan2")]
      ExpC z -> complex1 z complexExp
      LogC z -> complex1 z complexLog
      CosC z -> complex1 z $ \(x, y) -> do
        cosX <- fn "cos" x; coshY <- fn "cosh" y; sinX <- fn "sin" x; sinhY <- fn "sinh" y
        LComplex <$> realOf [localGet cosX, localGet coshY, f64Mul]
                 <*> realOf [f64Const 0, localGet sinX, localGet sinhY, f64Mul, f64Sub]
      SinC z -> complex1 z $ \(x, y) -> do
        cosX <- fn "cos" x; coshY <- fn "cosh" y; sinX <- fn "sin" x; sinhY <- fn "sinh" y
        LComplex <$> realOf [localGet sinX, localGet coshY, f64Mul]
                 <*> realOf [localGet cosX, localGet sinhY, f64Mul]
      TanC z -> complex1 z $ \(x, y) -> do
        tanX <- fn "tan" x; tanhY <- fn "tanh" y
        tanDiv tanX tanhY False
      SinhC z -> complex1 z $ \(x, y) -> do
        sinhX <- fn "sinh" x; cosY <- fn "cos" y; coshX <- fn "cosh" x; sinY <- fn "sin" y
        LComplex <$> realOf [localGet sinhX, localGet cosY, f64Mul]
                 <*> realOf [localGet coshX, localGet sinY, f64Mul]
      CoshC z -> complex1 z $ \(x, y) -> do
        coshX <- fn "cosh" x; cosY <- fn "cos" y; sinhX <- fn "sinh" x; sinY <- fn "sin" y
        LComplex <$> realOf [localGet coshX, localGet cosY, f64Mul]
                 <*> realOf [localGet sinhX, localGet sinY, f64Mul]
      TanhC z -> complex1 z $ \(x, y) -> do
        tanhX <- fn "tanh" x; tanY <- fn "tan" y
        tanDiv tanhX tanY True
      PowC mz mp -> do
        z <- mz; p <- mp
        case (z, p) of
          (LComplex zx zy, LComplex px py) -> lift (complexPow (zx, zy) (px, py))
          _ -> lift (mismatch z)
      SqrtC z -> complex1 z complexSqrt

      List ty mElems -> do
        elems <- sequence mElems
        lift (listLiteral ty elems)
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
      ConcatText{} -> lift (unsupported "text")
      ToText{} -> lift (unsupported "text")
      -- looked up in the kernel's copy of the snapshot (a point to add goes
      -- on its list of misses: see Backend.Wasm)
      Autocolor mode n mp -> mp >>= \case
        LComplex x y -> lift $ do
          AutocolorParams{..} <- getAutocolor
          rgb <- unaryTo I32 id
            [ localGet apSnapshot, i32Const (autocolorModeCode mode), i32Const (fromIntegral n)
            , localGet x, localGet y, localGet apMisses, call autocolorLookupFn ]
          let channel k = unaryTo I32 id [ localGet rgb, i32Const k, i32ShrU, i32Const 255, i32And ]
          LColor <$> channel 16 <*> channel 8 <*> channel 0
        l -> lift (mismatch l)
      TableIndex{} -> lift (unsupported "tables")
      TablePoint{} -> lift (unsupported "tables")
      TableSlice{} -> lift (unsupported "tables")
      TableSize{} -> lift (unsupported "tables")
      TableContains{} -> lift (unsupported "tables")
      TableCell{} -> lift (unsupported "tables")
      TableCellPoint{} -> lift (unsupported "tables")

------------------------------------------------------------------------
-- Helpers

captureR :: ReaderT r Gen a -> ReaderT r Gen (a, [Instr])
captureR m = ReaderT (capture . runReaderT m)

mismatch :: Loc -> Gen a
mismatch l = throwError ("internal error: a value of the wrong shape in the wasm backend: " ++ show l)

copied :: Loc -> Gen Loc
copied x = do
  x' <- freshLike x
  copyInto x x'
  pure x'

-- | Run instructions leaving one value of the given type, into a fresh
-- local.
unaryTo :: ValType -> (Int -> a) -> [Instr] -> Gen a
unaryTo t wrap is = do
  r <- newLocal t
  emit (is ++ [localSet r])
  pure (wrap r)

realOf :: [Instr] -> Gen Int
realOf = unaryTo F64 id

fn :: String -> Int -> Gen Int
fn f x = realOf [localGet x, call (mathFn f)]

constant :: TypeProxy t -> HaskellType t -> Gen Loc
constant t c = case t of
  BooleanType -> unaryTo I32 LBool [i32Const (if c then 1 else 0)]
  -- (integers are 32 bits in compiled code, as in the LLVM backend)
  IntegerType -> unaryTo I32 LInt [i32Const (fromIntegral (fromIntegral c :: Int32))]
  RealType -> unaryTo F64 LReal [f64Const c]
  ComplexType -> let x :+ y = c in LComplex <$> realOf [f64Const x] <*> realOf [f64Const y]
  ColorType -> do
    let (r, g, b) = colorToRGB c
        k v = unaryTo I32 id [i32Const (fromIntegral v)]
    LColor <$> k r <*> k g <*> k b
  PairType t1 t2 -> let (x, y) = c in LPair <$> constant t1 x <*> constant t2 y
  _ -> unsupported ("constants of type " ++ showType t)

-- | A color component in [0, 1] as a byte: clamped (NaN counts as 0), and
-- rounded to the nearest, as the LLVM backend does.
byte :: Loc -> Gen Int
byte = \case
  LReal c -> do
    c0 <- realOf [localGet c, f64Const 0, localGet c, f64Const 0, f64Gt, select]
    c1 <- realOf [localGet c0, f64Const 1, localGet c0, f64Const 1, f64Lt, select]
    unaryTo I32 id [localGet c1, f64Const 255, f64Mul, f64Const 0.5, f64Add, i32TruncSatF64U]
  l -> mismatch l

bool2 :: Instr -> ReaderT r Gen Loc -> ReaderT r Gen Loc -> ReaderT r Gen Loc
bool2 o x y = do
  a <- x; b <- y
  case (a, b) of
    (LBool p, LBool q) -> lift (unaryTo I32 LBool [localGet p, localGet q, o])
    _ -> lift (mismatch a)

int2 :: (Int -> Loc) -> Instr -> ReaderT r Gen Loc -> ReaderT r Gen Loc -> ReaderT r Gen Loc
int2 wrap o x y = do
  a <- x; b <- y
  case (a, b) of
    (LInt p, LInt q) -> lift (unaryTo I32 wrap [localGet p, localGet q, o])
    _ -> lift (mismatch a)

real2 :: Instr -> ReaderT r Gen Loc -> ReaderT r Gen Loc -> ReaderT r Gen Loc
real2 o x y = do
  a <- x; b <- y
  case (a, b) of
    (LReal p, LReal q) -> lift (unaryTo F64 LReal [localGet p, localGet q, o])
    _ -> lift (mismatch a)

real1To :: ValType -> (Int -> Loc) -> ReaderT r Gen Loc -> [Instr] -> ReaderT r Gen Loc
real1To t wrap r is = r >>= \case
  LReal x -> lift (unaryTo t wrap (localGet x : is))
  l -> lift (mismatch l)

math1 :: String -> ReaderT r Gen Loc -> ReaderT r Gen Loc
math1 f r = real1To F64 LReal r [call (mathFn f)]

complex1 :: ReaderT r Gen Loc -> ((Int, Int) -> Gen Loc) -> ReaderT r Gen Loc
complex1 z k = z >>= \case
  LComplex x y -> lift (k (x, y))
  l -> lift (mismatch l)

complex2 :: ReaderT r Gen Loc -> ReaderT r Gen Loc
         -> ((Int, Int) -> (Int, Int) -> Gen Loc) -> ReaderT r Gen Loc
complex2 x y k = do
  a <- x; b <- y
  case (a, b) of
    (LComplex p q, LComplex r s) -> lift (k (p, q) (r, s))
    _ -> lift (mismatch a)

complexMul :: (Int, Int) -> (Int, Int) -> Gen Loc
complexMul (lx, ly) (rx, ry) = do
  xx <- realOf [localGet lx, localGet rx, f64Mul]
  yy <- realOf [localGet ly, localGet ry, f64Mul]
  xy <- realOf [localGet lx, localGet ry, f64Mul]
  yx <- realOf [localGet ly, localGet rx, f64Mul]
  LComplex <$> realOf [localGet xx, localGet yy, f64Sub]
           <*> realOf [localGet xy, localGet yx, f64Add]

complexDiv :: (Int, Int) -> (Int, Int) -> Gen Loc
complexDiv (lx, ly) (rx, ry) = do
  r2 <- realOf [localGet rx, localGet rx, f64Mul, localGet ry, localGet ry, f64Mul, f64Add]
  xx <- realOf [localGet lx, localGet rx, f64Mul]
  yy <- realOf [localGet ly, localGet ry, f64Mul]
  xy <- realOf [localGet lx, localGet ry, f64Mul]
  yx <- realOf [localGet ly, localGet rx, f64Mul]
  LComplex <$> realOf [localGet xx, localGet yy, f64Add, localGet r2, f64Div]
           <*> realOf [localGet yx, localGet xy, f64Sub, localGet r2, f64Div]

complexExp :: (Int, Int) -> Gen Loc
complexExp (x, y) = do
  ex <- fn "exp" x; cy <- fn "cos" y; sy <- fn "sin" y
  LComplex <$> realOf [localGet ex, localGet cy, f64Mul]
           <*> realOf [localGet ex, localGet sy, f64Mul]

complexLog :: (Int, Int) -> Gen Loc
complexLog (x, y) = do
  norm2 <- realOf [localGet x, localGet x, f64Mul, localGet y, localGet y, f64Mul, f64Add]
  LComplex <$> realOf [f64Const 0.5, localGet norm2, call (mathFn "log"), f64Mul]
           <*> realOf [localGet y, localGet x, call (mathFn "atan2")]

-- | tan(x + iy) from tan x and tanh y (or tanh(x + iy), from tanh x and
-- tan y), as the LLVM backend computes them.
tanDiv :: Int -> Int -> Bool -> Gen Loc
tanDiv a b hyperbolic = do
  ab <- realOf [localGet a, localGet b, f64Mul]
  a2b <- realOf [localGet a, localGet ab, f64Mul]
  ab2 <- realOf [localGet b, localGet ab, f64Mul]
  a2b2 <- realOf [localGet a, localGet ab2, f64Mul]
  den <- realOf [f64Const 1, localGet a2b2, f64Add]
  re <- realOf [localGet a, localGet ab2, if hyperbolic then f64Add else f64Sub]
  im <- realOf [localGet b, localGet a2b, if hyperbolic then f64Sub else f64Add]
  LComplex <$> realOf [localGet re, localGet den, f64Div]
           <*> realOf [localGet im, localGet den, f64Div]

-- | z^p, as exp(p log z), except where the interpreter (Data.Complex's
-- (**)) differs: z^0 is 1, and at z = 0 the result is 0, inf, or NaN as
-- Re p is positive, negative, or zero.
complexPow :: (Int, Int) -> (Int, Int) -> Gen Loc
complexPow (zx, zy) (px, py) = do
  l <- complexLog (zx, zy)
  m <- case l of { LComplex a b -> complexMul (px, py) (a, b); _ -> mismatch l }
  g <- case m of { LComplex a b -> complexExp (a, b); _ -> mismatch m }
  case g of
    LComplex gx gy -> do
      zIsZero <- bothZero zx zy
      pIsZero <- bothZero px py
      rePos <- unaryTo I32 id [localGet px, f64Const 0, f64Gt]
      reNeg <- unaryTo I32 id [localGet px, f64Const 0, f64Lt]
      let inf = 1 / 0; nan = 0 / 0
      -- (select a b c is: if c then a else b)
      atZeroX <- realOf [f64Const 0, f64Const inf, f64Const nan, localGet reNeg, select, localGet rePos, select]
      atZeroY <- realOf [f64Const 0, f64Const 0, f64Const nan, localGet reNeg, select, localGet rePos, select]
      rx <- realOf [ f64Const 1, localGet atZeroX, localGet gx, localGet zIsZero, select
                   , localGet pIsZero, select ]
      ry <- realOf [ f64Const 0, localGet atZeroY, localGet gy, localGet zIsZero, select
                   , localGet pIsZero, select ]
      pure (LComplex rx ry)
    _ -> mismatch g

-- | The principal square root, as Data.Complex's sqrt computes it.
complexSqrt :: (Int, Int) -> Gen Loc
complexSqrt (x, y) = do
  norm <- realOf [localGet x, localGet y, call (mathFn "hypot")]
  absX <- realOf [localGet x, f64Abs]
  absY <- realOf [localGet y, f64Abs]
  u <- realOf [f64Const 0.5, localGet norm, localGet absX, f64Add, f64Mul, f64Sqrt]
  v <- realOf [localGet absY, f64Const 2, localGet u, f64Mul, f64Div]
  xNeg <- unaryTo I32 id [localGet x, f64Const 0, f64Lt]
  re <- realOf [localGet v, localGet u, localGet xNeg, select]
  im0 <- realOf [localGet u, localGet v, localGet xNeg, select]
  im <- realOf [ f64Const 0, localGet im0, f64Sub, localGet im0
               , localGet y, f64Const 0, f64Lt, select ]
  isZero <- bothZero x y
  LComplex <$> realOf [f64Const 0, localGet re, localGet isZero, select]
           <*> realOf [f64Const 0, localGet im, localGet isZero, select]

bothZero :: Int -> Int -> Gen Int
bothZero x y = unaryTo I32 id
  [localGet x, f64Const 0, f64Eq, localGet y, f64Const 0, f64Eq, i32And]

equal :: TypeProxy t -> Loc -> Loc -> Gen Loc
equal t a b = case (t, a, b) of
  (BooleanType, LBool p, LBool q) -> cmp [localGet p, localGet q, i32Eq]
  (IntegerType, LInt p, LInt q) -> cmp [localGet p, localGet q, i32Eq]
  (RealType, LReal p, LReal q) -> cmp [localGet p, localGet q, f64Eq]
  (ComplexType, LComplex p q, LComplex r s) ->
    cmp [localGet p, localGet r, f64Eq, localGet q, localGet s, f64Eq, i32And]
  (ColorType, LColor r g bl, LColor r' g' b') ->
    cmp [ localGet r, localGet r', i32Eq, localGet g, localGet g', i32Eq, i32And
        , localGet bl, localGet b', i32Eq, i32And ]
  (PairType t1 t2, LPair x1 x2, LPair y1 y2) -> do
    c1 <- equal t1 x1 y1
    c2 <- equal t2 x2 y2
    case (c1, c2) of
      (LBool p, LBool q) -> cmp [localGet p, localGet q, i32And]
      _ -> mismatch c1
  _ -> unsupported ("equality comparisons of type " ++ showType t)
  where cmp = unaryTo I32 LBool

-- | Inequality. For real numbers it's LLVM's "ordered and not equal" (so
-- NaN is unequal to nothing), unlike wasm's f64.ne.
notEqual :: TypeProxy t -> Loc -> Loc -> Gen Loc
notEqual t a b = case (t, a, b) of
  (BooleanType, LBool p, LBool q) -> cmp [localGet p, localGet q, i32Ne]
  (IntegerType, LInt p, LInt q) -> cmp [localGet p, localGet q, i32Ne]
  (RealType, LReal p, LReal q) -> cmp (one p q)
  (ComplexType, LComplex p q, LComplex r s) -> cmp (one p r ++ one q s ++ [i32Or])
  (ColorType, LColor r g bl, LColor r' g' b') ->
    cmp [ localGet r, localGet r', i32Ne, localGet g, localGet g', i32Ne, i32Or
        , localGet bl, localGet b', i32Ne, i32Or ]
  (PairType t1 t2, LPair x1 x2, LPair y1 y2) -> do
    c1 <- notEqual t1 x1 y1
    c2 <- notEqual t2 x2 y2
    case (c1, c2) of
      (LBool p, LBool q) -> cmp [localGet p, localGet q, i32Or]
      _ -> mismatch c1
  _ -> unsupported ("inequality comparisons of type " ++ showType t)
  where
    cmp = unaryTo I32 LBool
    one p q = [localGet p, localGet q, f64Lt, localGet p, localGet q, f64Gt, i32Or]

-- | Integer division or remainder. Where wasm would trap, this gives what
-- the LLVM backend's code gives on 64-bit ARM: dividing by 0 gives 0 (and
-- the remainder is the dividend), and the smallest integer divided by -1
-- is itself (remainder 0).
intDivision :: Bool -> Loc -> Loc -> Gen Loc
intDivision isDiv a b = case (a, b) of
  (LInt x, LInt y) -> do
    r <- newLocal I32
    emit [ localGet y, i32Eqz
         , IfElse [ if isDiv then i32Const 0 else localGet x, localSet r ]
             [ localGet y, i32Const (-1), i32Eq
             , IfElse (if isDiv then [i32Const 0, localGet x, i32Sub, localSet r]
                                else [i32Const 0, localSet r])
                 [ localGet x, localGet y, if isDiv then i32DivS else i32RemS, localSet r ]
             ]
         ]
    pure (LInt r)
  _ -> mismatch a

-- | An integer power by repeated multiplication, as the LLVM backend: a
-- negative power is 0, except of 1 (1) and of -1 (1 or -1).
intPow :: Int -> Int -> Gen Loc
intPow base expo = do
  result <- newLocal I32
  e <- newLocal I32
  emit [ i32Const 1, localSet result, localGet expo, localSet e
       , Block [ Loop [ localGet e, i32Const 0, i32LeS, brIf 1
                      , localGet result, localGet base, i32Mul, localSet result
                      , localGet e, i32Const 1, i32Sub, localSet e
                      , br 0 ] ] ]
  -- fraction = base == 1 ? 1 : (base == -1 ? (odd expo ? -1 : 1) : 0)
  minusOnePower <- unaryTo I32 id
    [ i32Const (-1), i32Const 1, localGet expo, i32Const 1, i32And, select ]
  fraction <- unaryTo I32 id
    [ i32Const 1
    , localGet minusOnePower, i32Const 0, localGet base, i32Const (-1), i32Eq, select
    , localGet base, i32Const 1, i32Eq, select ]
  unaryTo I32 LInt
    [ localGet fraction, localGet result, localGet expo, i32Const 0, i32LtS, select ]
