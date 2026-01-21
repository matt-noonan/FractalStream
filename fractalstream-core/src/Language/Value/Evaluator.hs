{-# language AllowAmbiguousTypes, UndecidableInstances #-}

module Language.Value.Evaluator
  ( evaluate
  , evaluator
  , HaskellType_
  , HaskellValue
  , partialEvaluate
  , constantFold
  , evaluateInContext
  , type ScalarFromContext
  ) where

import FractalStream.Prelude

import Language.Value
import Data.Indexed.Functor
import Data.Color

import Unsafe.Coerce
import Numeric.Extras

-- | First-class family corresponding to 'HaskellType', suitable to use in a 'Context'
data HaskellValue :: Symbol -> FSType -> Exp Type
type instance Eval (HaskellValue name t) = HaskellType t

-- | First-class family corresponding to 'HaskellType', suitable to use in a 'Context'
data HaskellType_ :: (Environment, FSType) -> Exp Type
type instance Eval (HaskellType_ et) = HaskellType (Ty et)

type family WithoutBinding (env :: Environment) (name :: Symbol) :: Environment where
  WithoutBinding ( '(name,t) ': env ) name = env
  WithoutBinding ( '(x,t) ': env )    name = '(x,t) ': WithoutBinding env name

-- | Perform partial evaluation, replacing uses of a specific
-- variable with the given value.
--
-- NOTE: This actually computes a Value env t -> Value env t transformation,
-- but hits the result with unsafeCoerce to transform env to
-- (env `WithoutBinding name). This is safe because the environment only
-- appears in the `NameIsPresent` proof in the Var
-- constructor, and this proof doesn't carry any nontrivial
-- information. Since all references to 'name' are removed by this
-- transformation, the result type really *is* correct.
partialEvaluate :: forall env t name ty
                 . (KnownSymbol name)
                => Proxy name
                -> TypeProxy ty
                -> HaskellType ty
                -> NameIsPresent name ty env
                -> Value '(env, t)
                -> Value '(env `WithoutBinding` name, t)
partialEvaluate name ty v _pf =
  unsafeCoerce . (indexedFold @(FIX ValueF) $ \case

    Var name' ty' pf' -> case sameSymbol name name' of
      Just Refl -> case sameHaskellType ty ty' of -- was: case typeIsUnique pf pf' of
        Just Refl -> Const (Scalar ty v)
        -- The invariant is: "if X is bound to type T at the outer scope, and X
        -- is referenced within the value, that reference will *also* be at type T.
        -- In other words: we don't have name shadowing, so the environment only grows.
        Nothing   -> error "internal error: should be unreachable"
      Nothing   -> Var name' ty' pf'
    etc -> etc)

-- | ValueOrConstant is used during constant folding. It
-- can represent either a value of type @Value env t@,
-- or else a constant of type @HaskellType t@.
data ValueOrConstant (et :: (Environment, FSType)) where
  V :: Value et -> ValueOrConstant et
  C :: KnownEnvironment env => Scalar t -> ValueOrConstant '(env, t)

-- | Turn a ValueOrConstant into a Value. This can
-- always be done.
toV :: ValueOrConstant et -> Value et
toV = \case
  V v -> v
  C c -> Const c

-- | Turn a ValueOrConstant into a constant. This
-- could fail, hence the Maybe.
toC :: ValueOrConstant et -> Maybe (HaskellType (Ty et))
toC = \case { V _ -> Nothing; C (Scalar _ c) -> Just c }

envTypeToType :: EnvTypeProxy '(env, t) -> TypeProxy t
envTypeToType (EnvType t) = t

-- | Perform a constant-folding transformation over a 'Value'
constantFold :: forall et. Value et -> Value et
constantFold =
  toV . (indexedFold @(Pure1 ValueOrConstant) $ \case
    -- Base cases: constants are constant, variables aren't.
    Const c     -> C c
    Var name ty pf -> V (Var name ty pf)

    -- Special cases, where we can constant-fold even with variables
    MulI (C x@(Scalar _ 0)) _ -> C x
    MulI _ (C x@(Scalar _ 0)) -> C x
    MulF (C x@(Scalar _ 0)) _ -> C x
    MulF _ (C x@(Scalar _ 0)) -> C x
    MulC (C x@(Scalar _ 0)) _ -> C x
    MulC _ (C x@(Scalar _ 0)) -> C x
    DivI (C x@(Scalar _ 0)) _ -> C x
    DivF (C x@(Scalar _ 0)) _ -> C x
    DivC (C x@(Scalar _ 0)) _ -> C x

    -- For all other constructors: if all children are constants,
    -- use the 'evaluator' algebra to compute the value of the constructor.
    -- If not all children are constants, create a non-constant Value.
    etc -> case itraverse @_ @_ @_ @_ @(Pure1 ValueOrConstant) @HaskellType_ (const toC) etc of
        Nothing -> V (imap (const toV) etc)
        Just  c -> case (lemmaEnvTy' (toIndex etc), toIndex etc) of
          (Refl, EnvType _) -> C (Scalar (envTypeToType (toIndex etc))
                                  (evaluator (imap (\_ t _ -> t) c) impossible))
        )

 where
   impossible = error "unreachable, Var constructor is already handled in constantFold"

-- | First-class family corresponding to 'HaskellType', suitable to use in a 'Context'
data ScalarFromContext :: (Environment, FSType) -> Exp Type
type instance Eval (ScalarFromContext et) =
  Context HaskellValue (Env et) -> HaskellType (Ty et)

evaluateInContext :: forall env t
                   . Context HaskellValue env
                  -> Value '(env, t)
                  -> HaskellType t
evaluateInContext = flip evaluate


-- | Evaluate the (normal) value corresponding to a 'Value', given values
-- for each variable that appears.
evaluate :: forall env t
          . Value '(env, t)
         -> Context HaskellValue env
         -> HaskellType t
evaluate =
  indexedFold @ScalarFromContext (\v ->
    case lemmaEnvTy' (toIndex v) of
      Refl -> evaluator v)

-- | Evaluation algebra
evaluator :: forall et
           . ValueF ScalarFromContext et
          -> Context HaskellValue (Env et)
          -> HaskellType (Ty et)
evaluator v0 ctx = case v0 of

    Const (Scalar _ v) -> v
    Var _name ty pf -> withKnownType ty (getBinding ctx pf)
    LocalLet name vty pf v _ e ->
      e (recallIsAbsent pf (Bind name vty (v ctx) ctx))

    PairV _ x y -> (x ctx, y ctx)
    ProjV1 _ p  -> let (x, _) = p ctx in x
    ProjV2 _ p  -> let (_, y) = p ctx in y

    List _ xs -> map ($ ctx) xs

    AddF x y -> x ctx + y ctx
    SubF x y -> x ctx - y ctx
    MulF x y -> x ctx * y ctx
    DivF x y -> x ctx / y ctx
    ModF x y ->
      let y' = y ctx
          m = fmod (x ctx) (y ctx)
      in if m < 0 then m + y' else m

    PowF x n -> x ctx ** n ctx
    AbsF x   -> abs (x ctx)
    NegF x   -> negate (x ctx)

    ExpF x -> exp (x ctx)
    LogF x -> log (x ctx)
    SqrtF x -> sqrt (x ctx)

    SinF x -> sin (x ctx)
    CosF x -> cos (x ctx)
    TanF x -> tan (x ctx)
    ArcsinF x -> asin (x ctx)
    ArccosF x -> acos (x ctx)
    ArctanF x -> atan (x ctx)
    Arctan2F x y -> atan2 (x ctx) (y ctx)

    SinhF x -> sinh (x ctx)
    CoshF x -> cosh (x ctx)
    TanhF x -> tanh (x ctx)
    ArcsinhF x -> asinh (x ctx)
    ArccoshF x -> acosh (x ctx)
    ArctanhF x -> atanh (x ctx)

    AddC x y -> x ctx + y ctx
    SubC x y -> x ctx - y ctx
    MulC x y -> x ctx * y ctx
    DivC x y -> x ctx / y ctx
    PowC x n -> x ctx ** n ctx
    NegC x   -> negate (x ctx)

    ExpC x -> exp (x ctx)
    LogC x -> log (x ctx)
    SqrtC x -> sqrt (x ctx)

    SinC x -> sin (x ctx)
    CosC x -> cos (x ctx)
    TanC x -> tan (x ctx)

    SinhC x -> sinh (x ctx)
    CoshC x -> cosh (x ctx)
    TanhC x -> tanh (x ctx)

    AbsC z  -> let x :+ y = z ctx in sqrt (x ** 2 + y ** 2)
    ArgC z  -> let x :+ y = z ctx in atan2 y x
    ReC z   -> let x :+ _ = z ctx in x
    ImC z   -> let _ :+ y = z ctx in y
    ConjC z -> let x :+ y = z ctx in x :+ negate y

    I2R n -> fromIntegral (n ctx)
    R2C x -> x ctx :+ 0
    C2R2 z -> let x :+ y = z ctx
              in (x, y)
    ToText t x -> showValue t (x ctx)

    AddI x y -> x ctx + y ctx
    SubI x y -> x ctx - y ctx
    MulI x y -> x ctx * y ctx
    DivI x y ->
      let yy = y ctx
      in if yy == 0
         then error "Integer division by 0"
         else x ctx `div` yy
    ModI x y ->
      let yy = y ctx
      in if yy == 0
         then error "Integer modulo by 0"
         else x ctx `mod` yy
    PowI x y -> case (x ctx, y ctx) of
      (_, 0) -> 1
      (1, _) -> 1
      (n, p) | p < 0     -> 0
             | otherwise -> n ^ p
    AbsI x   -> abs (x ctx)
    NegI x   -> negate (x ctx)

    Or  x y -> x ctx || y ctx
    And x y -> x ctx && y ctx
    Not x   -> not (x ctx)

    ITE _ b yes no -> if b ctx then yes ctx else no ctx

    RGB r g b     -> rgbToColor ( round (255 * r ctx)
                                , round (255 * g ctx)
                                , round (255 * b ctx) )
    Blend s c1 c2 -> mixColors (s ctx) (c1 ctx) (c2 ctx)

    InvertRGB c   -> invertColor (c ctx)

    Eql t x y -> Scalar t (x ctx) == Scalar t (y ctx)
    NEq t x y -> Scalar t (x ctx) /= Scalar t (y ctx)

    LTI x y -> x ctx < y ctx
    LTF x y -> x ctx < y ctx

    ConcatText xs -> concatMap ($ ctx) xs

    Join _ xss -> concatMap ($ ctx) xss

    Remove n ty pf xs test ->
      [ x | x <- xs ctx, not $ test (recallIsAbsent pf (Bind n ty x ctx)) ]

    Find n ty pf xs test x0 -> fromMaybe (x0 ctx) . listToMaybe $
      [ x | x <- xs ctx, test (recallIsAbsent pf (Bind n ty x ctx)) ]

    Transform n t1 _ pf xs f ->
      [ f (recallIsAbsent pf (Bind n t1 x ctx)) | x <- xs ctx ]

    Range lo hi -> [lo ctx .. hi ctx]

    Index _ False xs_ i_ ->
      let xs = xs_ ctx
          i  = fromIntegral $ i_ ctx
      in case xs of
        [] -> error "Attempted to index into an empty list"
        _  | i == 0    -> error ("Attempted to access list index 0, but lists use 1-based indexing")
           | i <= 0    -> if -i > length xs
                          then error ("List index " ++ show i ++ " is out of range")
                          else (xs !! (length xs + i))
           | otherwise -> if i > length xs
                          then error ("List index " ++ show i ++ " is out of range")
                          else (xs !! (i - 1))

    Index _ True xs_ i_ ->
      let xs = xs_ ctx
          len = length xs
          i0 = fromIntegral (i_ ctx - 1) `mod` len
          i = 1 + (if i0 < 0 then i0 + length xs else i0)
      in case xs of
        [] -> error "Attempted to cyclically index into an empty list"
        _  -> xs !! (i - 1)

    Length _ xs -> fromIntegral $ length (xs ctx)
