{-# language AllowAmbiguousTypes, UndecidableInstances #-}

module Language.Value
  ( module Language.Type
  , module Language.Environment
  , type Value
  , ValueF(..)
  , Value_
  , Proxy(..)
  , typeOfValue
  , get
  , pprint
  ) where

import Data.Proxy (Proxy(..))
import Fcf
import GHC.TypeLits
import Data.Ratio

import Data.Indexed.Functor
import Language.Type
import Language.Environment

import Data.Type.Equality ((:~:)(..))
import Data.List (intercalate)

---------------------------------------------------------------------------------
-- Value
---------------------------------------------------------------------------------

type Value = Fix ValueF

data Value_ :: Environment -> Type -> Exp *
type instance Eval (Value_ env t) = Value '(env, t)

data ValueF (value :: (Environment, Type) -> Exp *) (et :: (Environment, Type)) where

  -- Constants and variables
  Const :: forall ty env value. KnownEnvironment env => Scalar ty -> ValueF value '(env, ty)
  Var :: forall name ty env value
       . (KnownEnvironment env, KnownSymbol name)
      => Proxy name
      -> ScalarProxy ty
      -> NameIsPresent name ty env
      -> ValueF value '(env, ty)

  -- Product types and projections
  PairV :: forall t1 t2 env value. KnownEnvironment env => ScalarProxy ('Pair t1 t2) -> Eval (value '(env, t1)) -> Eval (value '(env, t2)) -> ValueF value '(env, 'Pair t1 t2)
  ProjV1 :: forall t1 t2 env value. KnownEnvironment env => ScalarProxy ('Pair t1 t2) -> Eval (value '(env, 'Pair t1 t2)) -> ValueF value '(env, t1)
  ProjV2 :: forall t1 t2 env value. KnownEnvironment env => ScalarProxy ('Pair t1 t2) -> Eval (value '(env, 'Pair t1 t2)) -> ValueF value '(env, t2)

  -- Floating-point arithmetic
  AddF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  SubF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  MulF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  DivF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  ModF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  PowF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  AbsF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  NegF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)

  -- Exponential and logarithmic functions
  ExpF  :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  LogF  :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  SqrtF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)

  -- Trigonometric functions
  SinF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  CosF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  TanF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  SinhF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  CoshF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  TanhF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  ArcsinF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  ArccosF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  ArctanF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  Arctan2F :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  ArcsinhF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  ArccoshF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  ArctanhF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)

  -- Complex arithmetic
  AddC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)
  SubC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)
  MulC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)
  DivC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)
  PowC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)
  NegC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)

  -- Complex-only functions
  AbsC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'RealT)
  ArgC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'RealT)
  ReC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'RealT)
  ImC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'RealT)
  ConjC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)

  -- Complex exponential and logarithmic functions
  ExpC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)
  LogC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)
  SqrtC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)

  -- Complex trigonometric functions
  SinC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)
  CosC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)
  TanC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)
  SinhC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)
  CoshC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)
  TanhC :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'ComplexT)

  -- Integer arithmetic
  AddI :: forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'IntegerT)
  SubI :: forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'IntegerT)
  MulI :: forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'IntegerT)
  DivI :: forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'IntegerT)
  ModI :: forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'IntegerT)
  PowI :: forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'IntegerT)
  AbsI :: forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'IntegerT)
  NegI :: forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'IntegerT)

  -- Conversion
  I2R :: forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'RealT)
  R2C :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'ComplexT)

  -- Boolean operations
  Or  :: forall env value. KnownEnvironment env => Eval (value '(env, 'BooleanT)) -> Eval (value '(env, 'BooleanT)) -> ValueF value '(env, 'BooleanT)
  And :: forall env value. KnownEnvironment env => Eval (value '(env, 'BooleanT)) -> Eval (value '(env, 'BooleanT)) -> ValueF value '(env, 'BooleanT)
  Not :: forall env value. KnownEnvironment env => Eval (value '(env, 'BooleanT)) -> ValueF value '(env, 'BooleanT)

  -- If/then/else expression
  ITE :: forall env t value
       . KnownEnvironment env
      => ScalarProxy t
      -> Eval (value '(env, 'BooleanT))
      -> Eval (value '(env, t))
      -> Eval (value '(env, t))
      -> ValueF value '(env, t)

  -- Color operations
  RGB :: forall env value
       . KnownEnvironment env
      => Eval (value '(env, 'RealT))
      -> Eval (value '(env, 'RealT))
      -> Eval (value '(env, 'RealT))
      -> ValueF value '(env, 'ColorT)

  Blend :: forall env value
         . KnownEnvironment env
        => Eval (value '(env, 'RealT))
        -> Eval (value '(env, 'ColorT))
        -> Eval (value '(env, 'ColorT))
        -> ValueF value '(env, 'ColorT)

  InvertRGB :: forall env value
             . KnownEnvironment env
            => Eval (value '(env, 'ColorT))
            -> ValueF value '(env, 'ColorT)

  -- Equality tests
  Eql :: forall env t value. KnownEnvironment env => ScalarProxy t -> Eval (value '(env, t)) -> Eval (value '(env, t)) -> ValueF value '(env, 'BooleanT)
  NEq :: forall env t value. KnownEnvironment env => ScalarProxy t -> Eval (value '(env, t)) -> Eval (value '(env, t)) -> ValueF value '(env, 'BooleanT)

  -- Scalar comparisons
  LEI :: forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'BooleanT)
  LEF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'BooleanT)
  GEI :: forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'BooleanT)
  GEF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'BooleanT)
  LTI :: forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'BooleanT)
  LTF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'BooleanT)
  GTI :: forall env value. KnownEnvironment env => Eval (value '(env, 'IntegerT)) -> Eval (value '(env, 'IntegerT)) -> ValueF value '(env, 'BooleanT)
  GTF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'BooleanT)

fix2 :: (Value '(env, t) -> Value '(env, t) -> ValueF (Pure1 Value) '(env, t))
     ->  Value '(env, t) -> Value '(env, t) -> Value '(env, t)
fix2 (#) x y = Fix (x # y)

instance KnownEnvironment env => Num (Value '(env, 'IntegerT)) where
  (+) = fix2 AddI
  (-) = fix2 SubI
  (*) = fix2 MulI
  abs = Fix . AbsI
  negate = Fix . NegI
  fromInteger = Fix . Const . Scalar IntegerProxy . fromInteger
  signum = error "TODO"

instance KnownEnvironment env => Num (Value '(env, 'RealT)) where
  (+) = fix2 AddF
  (-) = fix2 SubF
  (*) = fix2 MulF
  abs = Fix . AbsF
  negate = Fix . NegF
  fromInteger = Fix . Const . Scalar RealProxy . fromInteger
  signum = error "TODO"

instance KnownEnvironment env => Fractional (Value '(env, 'RealT)) where
  (/) = fix2 DivF
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)

instance KnownEnvironment env => Floating (Value '(env, 'RealT)) where
  pi = Fix (Const (Scalar RealProxy pi))
  exp = Fix . ExpF
  log = Fix . LogF
  sin = Fix . SinF
  cos = Fix . CosF
  tan = Fix . TanF
  asin = Fix . ArcsinF
  acos = Fix . ArccosF
  atan = Fix . ArctanF
  sinh = Fix . SinhF
  cosh = Fix . CoshF
  tanh = Fix . TanhF
  asinh = error "TODO"
  acosh = error "TODO"
  atanh = error "TODO"

instance KnownEnvironment env => Num (Value '(env, 'ComplexT)) where
  (+) = fix2 AddC
  (-) = fix2 SubC
  (*) = fix2 MulC
  abs = Fix . R2C . Fix . AbsC
  negate = Fix . NegC
  fromInteger = Fix . Const . Scalar ComplexProxy . fromInteger
  signum = error "TODO"

instance KnownEnvironment env => Fractional (Value '(env, 'ComplexT)) where
  (/) = fix2 DivC
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)

instance KnownEnvironment env => Floating (Value '(env, 'ComplexT)) where
  pi = Fix (Const (Scalar ComplexProxy pi))
  exp = Fix . ExpC
  log = Fix . LogC
  sin = Fix . SinC
  cos = Fix . CosC
  tan = Fix . TanC
  asin = error "TODO"
  acos = error "TODO"
  atan = error "TODO"
  sinh = Fix . SinhC
  cosh = Fix . CoshC
  tanh = Fix . TanhC
  asinh = error "TODO"
  acosh = error "TODO"
  atanh = error "TODO"

---------------------------------------------------------------------------------
-- IFunctor instance
---------------------------------------------------------------------------------

typeOfValue :: forall env t. Value '(env, t) -> ScalarProxy t
typeOfValue v = case toIndex (unrollIx @_ @Value @ValueF v) of
  EnvType t -> t

instance IFunctor ValueF where

  type IndexProxy ValueF = EnvTypeProxy

  toIndex :: ValueF value et -> EnvTypeProxy et
  toIndex = \case
    Const (Scalar t _) -> EnvType t
    Var _ t _ -> EnvType t

    PairV t _ _ -> EnvType t
    ProjV1 t _ -> case t of { PairProxy t1 _ -> EnvType t1 }
    ProjV2 t _ -> case t of { PairProxy _ t2 -> EnvType t2 }

    AddF {} -> EnvType RealProxy
    SubF {} -> EnvType RealProxy
    MulF {} -> EnvType RealProxy
    DivF {} -> EnvType RealProxy
    ModF {} -> EnvType RealProxy
    PowF {} -> EnvType RealProxy
    AbsF {} -> EnvType RealProxy
    NegF {} -> EnvType RealProxy

    ExpF {} -> EnvType RealProxy
    LogF {} -> EnvType RealProxy
    SqrtF {} -> EnvType RealProxy

    SinF {} -> EnvType RealProxy
    CosF {} -> EnvType RealProxy
    TanF {} -> EnvType RealProxy
    ArcsinF {} -> EnvType RealProxy
    ArccosF {} -> EnvType RealProxy
    ArctanF {} -> EnvType RealProxy
    Arctan2F {} -> EnvType RealProxy
    SinhF {} -> EnvType RealProxy
    CoshF {} -> EnvType RealProxy
    TanhF {} -> EnvType RealProxy
    ArcsinhF {} -> EnvType RealProxy
    ArccoshF {} -> EnvType RealProxy
    ArctanhF {} -> EnvType RealProxy

    AddC {} -> EnvType ComplexProxy
    SubC {} -> EnvType ComplexProxy
    MulC {} -> EnvType ComplexProxy
    DivC {} -> EnvType ComplexProxy
    PowC {} -> EnvType ComplexProxy
    NegC {} -> EnvType ComplexProxy

    AbsC {} -> EnvType RealProxy
    ArgC {} -> EnvType RealProxy
    ReC {} -> EnvType RealProxy
    ImC {} -> EnvType RealProxy
    ConjC {} -> EnvType ComplexProxy

    ExpC {} -> EnvType ComplexProxy
    LogC {} -> EnvType ComplexProxy
    SqrtC {} -> EnvType ComplexProxy

    SinC {} -> EnvType ComplexProxy
    CosC {} -> EnvType ComplexProxy
    TanC {} -> EnvType ComplexProxy
    SinhC {} -> EnvType ComplexProxy
    CoshC {} -> EnvType ComplexProxy
    TanhC {} -> EnvType ComplexProxy

    AddI {} -> EnvType IntegerProxy
    SubI {} -> EnvType IntegerProxy
    MulI {} -> EnvType IntegerProxy
    DivI {} -> EnvType IntegerProxy
    ModI {} -> EnvType IntegerProxy
    PowI {} -> EnvType IntegerProxy
    AbsI {} -> EnvType IntegerProxy
    NegI {} -> EnvType IntegerProxy

    I2R {} -> EnvType RealProxy
    R2C {} -> EnvType ComplexProxy

    Or {} -> EnvType BooleanProxy
    And {} -> EnvType BooleanProxy
    Not {} -> EnvType BooleanProxy

    ITE t _ _ _ -> EnvType t

    RGB {}       -> EnvType ColorProxy
    Blend {}     -> EnvType ColorProxy
    InvertRGB {} -> EnvType ColorProxy

    Eql {} -> EnvType BooleanProxy
    NEq {} -> EnvType BooleanProxy

    LEI {} -> EnvType BooleanProxy
    LEF {} -> EnvType BooleanProxy
    GEI {} -> EnvType BooleanProxy
    GEF {} -> EnvType BooleanProxy
    LTI {} -> EnvType BooleanProxy
    LTF {} -> EnvType BooleanProxy
    GTI {} -> EnvType BooleanProxy
    GTF {} -> EnvType BooleanProxy

  imap :: forall a b i
        . (forall j. EnvTypeProxy j -> Eval (a j) -> Eval (b j))
       -> ValueF a i
       -> ValueF b i
  imap f v0  = case lemmaEnvTy @i of
    Refl -> let { env = withEnvType (toIndex v0) (\e _ -> e); withEnv :: ScalarProxy t -> EnvTypeProxy '(Env i, t); withEnv t = envTypeProxy env t } in case v0 of
      Const x -> Const x
      Var name v pf -> Var name v pf

      PairV t x y ->
        let (t1, t2) = case t of { PairProxy tx ty -> (tx, ty) }
        in PairV t (f (withEnv t1) x) (f (withEnv t2) y)
      ProjV1 t p -> ProjV1 t (f (withEnv t) p)
      ProjV2 t p -> ProjV2 t (f (withEnv t) p)

      AddF x y -> AddF (f (withEnv RealProxy) x) (f (withEnv RealProxy) y)

      SubF x y -> SubF (f (withEnv RealProxy) x) (f (withEnv RealProxy) y)
      MulF x y -> MulF (f (withEnv RealProxy) x) (f (withEnv RealProxy) y)
      DivF x y -> DivF (f (withEnv RealProxy) x) (f (withEnv RealProxy) y)
      ModF x y -> ModF (f (withEnv RealProxy) x) (f (withEnv RealProxy) y)
      PowF x y -> PowF (f (withEnv RealProxy) x) (f (withEnv RealProxy) y)
      AbsF x   -> AbsF (f (withEnv RealProxy) x)
      NegF x   -> NegF (f (withEnv RealProxy) x)

      ExpF x -> ExpF (f (withEnv RealProxy) x)
      LogF x -> LogF (f (withEnv RealProxy) x)
      SqrtF x -> SqrtF (f (withEnv RealProxy) x)

      SinF x -> SinF (f (withEnv RealProxy) x)
      CosF x -> CosF (f (withEnv RealProxy) x)
      TanF x -> TanF (f (withEnv RealProxy) x)
      ArcsinF x -> ArcsinF (f (withEnv RealProxy) x)
      ArccosF x -> ArccosF (f (withEnv RealProxy) x)
      ArctanF x -> ArctanF (f (withEnv RealProxy) x)
      Arctan2F x y -> Arctan2F (f (withEnv RealProxy) x) (f (withEnv RealProxy) y)
      SinhF x -> SinhF (f (withEnv RealProxy) x)
      CoshF x -> CoshF (f (withEnv RealProxy) x)
      TanhF x -> TanhF (f (withEnv RealProxy) x)
      ArcsinhF x -> ArcsinhF (f (withEnv RealProxy) x)
      ArccoshF x -> ArccoshF (f (withEnv RealProxy) x)
      ArctanhF x -> ArctanhF (f (withEnv RealProxy) x)

      AddC x y -> AddC (f (withEnv ComplexProxy) x) (f (withEnv ComplexProxy) y)
      SubC x y -> SubC (f (withEnv ComplexProxy) x) (f (withEnv ComplexProxy) y)
      MulC x y -> MulC (f (withEnv ComplexProxy) x) (f (withEnv ComplexProxy) y)
      DivC x y -> DivC (f (withEnv ComplexProxy) x) (f (withEnv ComplexProxy) y)
      PowC x y -> PowC (f (withEnv ComplexProxy) x) (f (withEnv ComplexProxy) y)
      NegC x   -> NegC (f (withEnv ComplexProxy) x)

      ExpC x -> ExpC (f (withEnv ComplexProxy) x)
      LogC x -> LogC (f (withEnv ComplexProxy) x)
      SqrtC x -> SqrtC (f (withEnv ComplexProxy) x)

      SinC x -> SinC (f (withEnv ComplexProxy) x)
      CosC x -> CosC (f (withEnv ComplexProxy) x)
      TanC x -> TanC (f (withEnv ComplexProxy) x)
      SinhC x -> SinhC (f (withEnv ComplexProxy) x)
      CoshC x -> CoshC (f (withEnv ComplexProxy) x)
      TanhC x -> TanhC (f (withEnv ComplexProxy) x)

      AbsC x -> AbsC (f (withEnv ComplexProxy) x)
      ArgC x -> ArgC (f (withEnv ComplexProxy) x)
      ReC x -> ReC (f (withEnv ComplexProxy) x)
      ImC x -> ImC (f (withEnv ComplexProxy) x)
      ConjC x -> ConjC (f (withEnv ComplexProxy) x)

      AddI x y -> AddI (f (withEnv IntegerProxy) x) (f (withEnv IntegerProxy) y)
      SubI x y -> SubI (f (withEnv IntegerProxy) x) (f (withEnv IntegerProxy) y)
      MulI x y -> MulI (f (withEnv IntegerProxy) x) (f (withEnv IntegerProxy) y)
      DivI x y -> DivI (f (withEnv IntegerProxy) x) (f (withEnv IntegerProxy) y)
      ModI x y -> ModI (f (withEnv IntegerProxy) x) (f (withEnv IntegerProxy) y)
      PowI x y -> PowI (f (withEnv IntegerProxy) x) (f (withEnv IntegerProxy) y)
      AbsI x   -> AbsI (f (withEnv IntegerProxy) x)
      NegI x   -> NegI (f (withEnv IntegerProxy) x)

      I2R x -> I2R (f (withEnv IntegerProxy) x)
      R2C x -> R2C (f (withEnv RealProxy) x)

      Or  x y -> Or  (f (withEnv BooleanProxy) x) (f (withEnv BooleanProxy) y)
      And x y -> And (f (withEnv BooleanProxy) x) (f (withEnv BooleanProxy) y)
      Not x -> Not (f (withEnv BooleanProxy) x)

      ITE t b yes no -> ITE t (f (withEnv BooleanProxy) b) (f (withEnv t) yes) (f (withEnv t) no)

      RGB r g b -> RGB (f (withEnv RealProxy) r) (f (withEnv RealProxy) g) (f (withEnv RealProxy) b)
      Blend s c1 c2 -> Blend (f (withEnv RealProxy) s) (f (withEnv ColorProxy) c1) (f (withEnv ColorProxy) c2)
      InvertRGB c -> InvertRGB (f (withEnv ColorProxy) c)

      Eql t x y -> Eql t (f (withEnv t) x) (f (withEnv t) y)
      NEq t x y -> NEq t (f (withEnv t) x) (f (withEnv t) y)

      LEI x y -> LEI (f (withEnv IntegerProxy) x) (f (withEnv IntegerProxy) y)
      LEF x y -> LEF (f (withEnv RealProxy)    x) (f (withEnv RealProxy)    y)
      GEI x y -> GEI (f (withEnv IntegerProxy) x) (f (withEnv IntegerProxy) y)
      GEF x y -> GEF (f (withEnv RealProxy)    x) (f (withEnv RealProxy)    y)
      LTI x y -> LTI (f (withEnv IntegerProxy) x) (f (withEnv IntegerProxy) y)
      LTF x y -> LTF (f (withEnv RealProxy)    x) (f (withEnv RealProxy)    y)
      GTI x y -> GTI (f (withEnv IntegerProxy) x) (f (withEnv IntegerProxy) y)
      GTF x y -> GTF (f (withEnv RealProxy)    x) (f (withEnv RealProxy)    y)

---------------------------------------------------------------------------------
-- Indexed traversable instance for values
---------------------------------------------------------------------------------

instance ITraversable ValueF  where
  isequence = \case
    Const x -> pure (Const x)
    Var name v pf -> pure (Var name v pf)

    PairV t mx my -> PairV t <$> mx <*> my
    ProjV1 t mp   -> ProjV1 t <$> mp
    ProjV2 t mp   -> ProjV2 t <$> mp

    AddF mx my -> AddF <$> mx <*> my
    SubF mx my -> SubF <$> mx <*> my
    MulF mx my -> MulF <$> mx <*> my
    DivF mx my -> DivF <$> mx <*> my
    ModF mx my -> ModF <$> mx <*> my
    PowF mx my -> PowF <$> mx <*> my
    AbsF mx    -> AbsF <$> mx
    NegF mx    -> NegF <$> mx

    ExpF mx -> ExpF <$> mx
    LogF mx -> LogF <$> mx
    SqrtF mx -> SqrtF <$> mx

    SinF mx -> SinF <$> mx
    CosF mx -> CosF <$> mx
    TanF mx -> TanF <$> mx
    ArcsinF mx -> ArcsinF <$> mx
    ArccosF mx -> ArccosF <$> mx
    ArctanF mx -> ArctanF <$> mx
    Arctan2F mx my -> Arctan2F <$> mx <*> my

    SinhF mx -> SinhF <$> mx
    CoshF mx -> CoshF <$> mx
    TanhF mx -> TanhF <$> mx
    ArcsinhF mx -> ArcsinhF <$> mx
    ArccoshF mx -> ArccoshF <$> mx
    ArctanhF mx -> ArctanhF <$> mx

    AddC mx my -> AddC <$> mx <*> my
    SubC mx my -> SubC <$> mx <*> my
    MulC mx my -> MulC <$> mx <*> my
    DivC mx my -> DivC <$> mx <*> my
    PowC mx my -> PowC <$> mx <*> my
    NegC mx    -> NegC <$> mx

    ExpC mx -> ExpC <$> mx
    LogC mx -> LogC <$> mx
    SqrtC mx -> SqrtC <$> mx

    SinC mx -> SinC <$> mx
    CosC mx -> CosC <$> mx
    TanC mx -> TanC <$> mx

    SinhC mx -> SinhC <$> mx
    CoshC mx -> CoshC <$> mx
    TanhC mx -> TanhC <$> mx

    AbsC mx -> AbsC <$> mx
    ArgC mx -> ArgC <$> mx
    ReC mx -> ReC <$> mx
    ImC mx -> ImC <$> mx
    ConjC mx -> ConjC <$> mx

    AddI mx my -> AddI <$> mx <*> my
    SubI mx my -> SubI <$> mx <*> my
    MulI mx my -> MulI <$> mx <*> my
    DivI mx my -> DivI <$> mx <*> my
    ModI mx my -> ModI <$> mx <*> my
    PowI mx my -> PowI <$> mx <*> my
    AbsI mx    -> AbsI <$> mx
    NegI mx    -> NegI <$> mx

    I2R mx -> I2R <$> mx
    R2C mx -> R2C <$> mx

    Or  mx my -> Or  <$> mx <*> my
    And mx my -> And <$> mx <*> my
    Not mx    -> Not <$> mx

    ITE t mb myes mno -> ITE t <$> mb <*> myes <*> mno

    RGB mr mg mb -> RGB <$> mr <*> mg <*> mb
    Blend ms mc1 mc2 -> Blend <$> ms <*> mc1 <*> mc2
    InvertRGB mc -> InvertRGB <$> mc

    Eql t mx my -> Eql t <$> mx <*> my
    NEq t mx my -> NEq t <$> mx <*> my

    LEI mx my -> LEI <$> mx <*> my
    LEF mx my -> LEF <$> mx <*> my
    GEI mx my -> GEI <$> mx <*> my
    GEF mx my -> GEF <$> mx <*> my
    LTI mx my -> LTI <$> mx <*> my
    LTF mx my -> LTF <$> mx <*> my
    GTI mx my -> GTI <$> mx <*> my
    GTF mx my -> GTF <$> mx <*> my


instance Show (Value et) where show = pprint

data PrecString :: (Environment, Type) -> Exp *
type instance Eval (PrecString et) = String -- (Int, String)

pprint :: Value et -> String
pprint = indexedFold @PrecString @Value @ValueF go
 where
  go :: forall et'. ValueF PrecString et' -> String
  go = \case
    Const (Scalar t c) -> case t of
      BooleanProxy -> show c
      IntegerProxy -> show c
      RealProxy    -> show c
      ComplexProxy -> show c
      RationalProxy -> show c
      ColorProxy -> show c
      VoidProxy  -> show c
      ImageProxy -> show c
      PairProxy (t1 :: ScalarProxy t1) (t2 :: ScalarProxy t2) ->
        let (x,y) = c
        in concat [ "(", go @'(Env et', t1) (Const (Scalar t1 x)), ", "
                  , go @'(Env et', t2) (Const (Scalar t2 y)), ")"]
    Var name _ _ -> symbolVal name
    PairV _ x y  -> concat ["(", x, ", ", y, ")"]
    ProjV1 _ p   -> concat ["(1st ", p, ")"]
    ProjV2 _ p   -> concat ["(2nd ", p, ")"]
    AddI x y -> binop "+" x y
    SubI x y -> binop "-" x y
    MulI x y -> binop "*" x y
    DivI x y -> binop "/" x y
    ModI x y -> binop "mod" x y
    PowI x y -> binop "^" x y
    AbsI x   -> "|" ++ x ++ "|"
    NegI x   -> "-" ++ x
    I2R x    -> x ++ ":R"
    R2C x    -> x ++ ":C"
    Or x y   -> binop "or" x y
    And x y  -> binop "and" x y
    Not x    -> "!" ++ x
    Eql _ x y -> binop "=" x y
    NEq _ x y -> binop "≠" x y
    LEI x y  -> binop "≤" x y
    LEF x y  -> binop "≤" x y
    LTI x y  -> binop "<" x y
    LTF x y  -> binop "<" x y
    GEI x y  -> binop "≥" x y
    GEF x y  -> binop "≥" x y
    GTI x y  -> binop ">" x y
    GTF x y  -> binop ">" x y
    ITE _ c x y -> unwords ["if", c, "then", x, "else", y]
    RGB r g b     -> fun "rgb" [r,g,b]
    Blend s c1 c2 -> fun "blend" [s,c1,c2]
    InvertRGB c   -> fun "invert" [c]
    AddF x y -> binop "+" x y
    SubF x y -> binop "-" x y
    MulF x y -> binop "*" x y
    DivF x y -> binop "/" x y
    ModF x y -> binop "mod" x y
    PowF x y -> binop "^" x y
    AbsF x   -> "|" ++ x ++ "|"
    NegF x   -> "-" ++ x
    ExpF x   -> fun "exp" [x]
    LogF x   -> fun "log" [x]
    SqrtF x  -> fun "sqrt" [x]
    CosF x   -> fun "cos" [x]
    SinF x   -> fun "sin" [x]
    TanF x   -> fun "tan" [x]
    CoshF x  -> fun "cosh" [x]
    SinhF x  -> fun "sinh" [x]
    TanhF x  -> fun "tanh" [x]
    ArccosF x   -> fun "acos" [x]
    ArcsinF x   -> fun "asin" [x]
    ArctanF x   -> fun "atan" [x]
    Arctan2F x y -> fun "atan2" [x,y]
    ArccoshF x  -> fun "acosh" [x]
    ArcsinhF x  -> fun "asinh" [x]
    ArctanhF x  -> fun "atanh" [x]
    AddC x y -> binop "+" x y
    SubC x y -> binop "-" x y
    MulC x y -> binop "*" x y
    DivC x y -> binop "/" x y
    PowC x y -> binop "^" x y
    AbsC x   -> "|" ++ x ++ "|"
    NegC x   -> "-" ++ x
    ExpC x   -> fun "exp" [x]
    LogC x   -> fun "log" [x]
    SqrtC x  -> fun "sqrt" [x]
    CosC x   -> fun "cos" [x]
    SinC x   -> fun "sin" [x]
    TanC x   -> fun "tan" [x]
    CoshC x  -> fun "cosh" [x]
    SinhC x  -> fun "sinh" [x]
    TanhC x  -> fun "tanh" [x]
    ReC x -> fun "re" [x]
    ImC x -> fun "im" [x]
    ArgC x -> fun "arg" [x]
    ConjC x -> fun "bar" [x]

  fun f args = concat [f, "(", intercalate ", " args, ")"]
  binop op x y = concat ["(", x, " ", op, " ", y, ")"]

---------------------------------------------------------------------------------
-- Utility functions
---------------------------------------------------------------------------------

-- | Get the value of a variable.
-- This is the same as using the 'Get' constructor directly,
-- except that it saves you the syntactic noise of using a
-- 'Proxy', by using type applications instead.
--
--     get @"foo" IntegerProxy
--
-- vs
--
--     Get (Proxy :: Proxy "foo") IntegerProxy
--
get :: forall name env ty
     . ( Required name env ~ ty, NotPresent name (env `Without` name)
       , KnownSymbol name, KnownEnvironment env)
    => ScalarProxy ty
    -> Value '(env, ty)
get ty = Fix (Var (Proxy @name) ty bindingEvidence)
