{-# language AllowAmbiguousTypes, UndecidableInstances #-}

module Language.Value
  ( module Language.Type
  , module Language.Environment
  , type Value
  , ValueF(..)
  , Value_
  , Proxy(..)
  , SomeValue(..)
  , SomeConstantValue(..)
  , typeOfValue
  , getValue
  , pprint
  ) where

import FractalStream.Prelude

import Data.Ratio

import Data.Indexed.Functor
import Language.Type
import Language.Environment

---------------------------------------------------------------------------------
-- Value
---------------------------------------------------------------------------------

--type Value = Fix ValueF

data Value_ :: Environment -> FSType -> Exp Type
type instance Eval (Value_ env t) = Value '(env, t)

data SomeValue where
  SomeValue :: forall env ty
             . EnvironmentProxy env
            -> TypeProxy ty
            -> Value '(env, ty)
            -> SomeValue

data SomeConstantValue where
  SomeConstantValue :: forall ty
                     . TypeProxy ty
                    -> Value '( '[], ty )
                    -> SomeConstantValue

type Value = ValueF (FIX ValueF)

data ValueF (value :: (Environment, FSType) -> Exp Type) (et :: (Environment, FSType)) where

  -- Constants and variables
  Const :: forall ty env value
         . KnownEnvironment env
        => Scalar ty
        -> ValueF value '(env, ty)

  Var :: forall name ty env value
       . (KnownEnvironment env, KnownSymbol name)
      => Proxy name
      -> TypeProxy ty
      -> NameIsPresent name ty env
      -> ValueF value '(env, ty)

  LocalLet :: forall name vty ty env value
            . (KnownEnvironment env, KnownSymbol name)
           => Proxy name
           -> TypeProxy vty
           -> NameIsAbsent name env
           -> Eval (value '(env, vty))
           -> TypeProxy ty
           -> Eval (value '( '(name, vty) ': env,  ty))
           -> ValueF value '(env, ty)

  -- Product types and projections
  PairV :: forall t1 t2 env value
         . KnownEnvironment env
        => TypeProxy ('Pair t1 t2)
        -> Eval (value '(env, t1))
        -> Eval (value '(env, t2))
        -> ValueF value '(env, 'Pair t1 t2)

  ProjV1 :: forall t1 t2 env value
          . KnownEnvironment env
         => TypeProxy ('Pair t1 t2)
         -> Eval (value '(env, 'Pair t1 t2))
         -> ValueF value '(env, t1)

  ProjV2 :: forall t1 t2 env value
          . KnownEnvironment env
         => TypeProxy ('Pair t1 t2)
         -> Eval (value '(env, 'Pair t1 t2))
         -> ValueF value '(env, t2)

  -- Lists
  List :: forall t env value
        . KnownEnvironment env
       => TypeProxy t
       -> [Eval (value '(env, t))]
       -> ValueF value '(env, 'ListT t)

  -- Floating-point arithmetic
  AddF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  SubF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  MulF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  DivF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  ModF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  PowF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  AbsF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)
  NegF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'RealT)

  -- Rounding
  RoundF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'IntegerT)
  FloorF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'IntegerT)
  CeilingF :: forall env value. KnownEnvironment env => Eval (value '(env, 'RealT)) -> ValueF value '(env, 'IntegerT)

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
  C2R2 :: forall env value. KnownEnvironment env => Eval (value '(env, 'ComplexT)) -> ValueF value '(env, 'Pair 'RealT 'RealT)
  ToText :: forall env ty value. KnownEnvironment env => TypeProxy ty -> Eval (value '(env, ty)) -> ValueF value '(env, 'TextT)

  -- Boolean operations
  Or  :: forall env value. KnownEnvironment env => Eval (value '(env, 'BooleanT)) -> Eval (value '(env, 'BooleanT)) -> ValueF value '(env, 'BooleanT)
  And :: forall env value. KnownEnvironment env => Eval (value '(env, 'BooleanT)) -> Eval (value '(env, 'BooleanT)) -> ValueF value '(env, 'BooleanT)
  Not :: forall env value. KnownEnvironment env => Eval (value '(env, 'BooleanT)) -> ValueF value '(env, 'BooleanT)

  -- If/then/else expression
  ITE :: forall env t value
       . KnownEnvironment env
      => TypeProxy t
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
  Eql :: forall env t value. KnownEnvironment env => TypeProxy t -> Eval (value '(env, t)) -> Eval (value '(env, t)) -> ValueF value '(env, 'BooleanT)
  NEq :: forall env t value. KnownEnvironment env => TypeProxy t -> Eval (value '(env, t)) -> Eval (value '(env, t)) -> ValueF value '(env, 'BooleanT)

  -- Scalar comparisons. We only keep < here, other comparisions should be
  -- transformed to < beforehand.
  LTI :: forall env value
       . KnownEnvironment env
      => Eval (value '(env, 'IntegerT))
      -> Eval (value '(env, 'IntegerT))
      -> ValueF value '(env, 'BooleanT)

  LTF :: forall env value
       . KnownEnvironment env
      => Eval (value '(env, 'RealT))
      -> Eval (value '(env, 'RealT))
      -> ValueF value '(env, 'BooleanT)

  -- Text operations
  ConcatText :: forall env value. KnownEnvironment env => [Eval (value '(env, 'TextT))] -> ValueF value '(env, 'TextT)

  -- List operations
  Join :: forall env ty value. KnownEnvironment env
       => TypeProxy ty
       -> [Eval (value '(env, 'ListT ty))]
       -> ValueF value '(env, 'ListT ty)

  Remove :: forall env ty name value. (KnownEnvironment env, KnownSymbol name)
         => Proxy name
         -> TypeProxy ty
         -> NameIsAbsent name env
         -> Eval (value '(env, 'ListT ty))
         -> Eval (value '( '(name, ty) ': env, 'BooleanT))
         -> ValueF value '(env, 'ListT ty)

  Find :: forall env ty name value. (KnownEnvironment env, KnownSymbol name)
       => Proxy name
       -> TypeProxy ty
       -> NameIsAbsent name env
       -> Eval (value '(env, 'ListT ty))
       -> Eval (value '( '(name, ty) ': env, 'BooleanT))
       -> Eval (value '(env, ty))
       -> ValueF value '(env, ty)

  Transform :: forall env ty1 ty2 name value. (KnownEnvironment env, KnownSymbol name)
            => Proxy name
            -> TypeProxy ty1
            -> TypeProxy ty2
            -> NameIsAbsent name env
            -> Eval (value '(env, 'ListT ty1))
            -> Eval (value '( '(name, ty1) ': env, ty2))
            -> ValueF value '(env, 'ListT ty2)

  Range :: forall env value. KnownEnvironment env
        => Eval (value '(env, 'IntegerT))
        -> Eval (value '(env, 'IntegerT))
        -> ValueF value '(env, 'ListT 'IntegerT)

  Length :: forall env ty value. KnownEnvironment env
         => TypeProxy ty
         -> Eval (value '(env, 'ListT ty))
         -> ValueF value '(env, 'IntegerT)

  Index :: forall env ty value. KnownEnvironment env
        => TypeProxy ty
        -> Bool -- use cyclic indexing?
        -> Eval (value '(env, 'ListT ty))
        -> Eval (value '(env, 'IntegerT))
        -> ValueF value '(env, ty)

instance KnownEnvironment env => Num (Value '(env, 'IntegerT)) where
  (+) = AddI
  (-) = SubI
  (*) = MulI
  abs = AbsI
  negate = NegI
  fromInteger = Const . Scalar IntegerType . fromInteger
  signum = error "TODO"

instance KnownEnvironment env => Num (Value '(env, 'RealT)) where
  (+) = AddF
  (-) = SubF
  (*) = MulF
  abs = AbsF
  negate = NegF
  fromInteger = Const . Scalar RealType . fromInteger
  signum = error "TODO"

instance KnownEnvironment env => Fractional (Value '(env, 'RealT)) where
  (/) = DivF
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)

instance KnownEnvironment env => Floating (Value '(env, 'RealT)) where
  pi = Const (Scalar RealType pi)
  exp = ExpF
  log = LogF
  sin = SinF
  cos = CosF
  tan = TanF
  asin = ArcsinF
  acos = ArccosF
  atan = ArctanF
  sinh = SinhF
  cosh = CoshF
  tanh = TanhF
  asinh = error "TODO"
  acosh = error "TODO"
  atanh = error "TODO"

instance KnownEnvironment env => Num (Value '(env, 'ComplexT)) where
  (+) = AddC
  (-) = SubC
  (*) = MulC
  abs = R2C . AbsC
  negate = NegC
  fromInteger = Const . Scalar ComplexType . fromInteger
  signum = error "TODO"

instance KnownEnvironment env => Fractional (Value '(env, 'ComplexT)) where
  (/) = DivC
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)

instance KnownEnvironment env => Floating (Value '(env, 'ComplexT)) where
  pi = Const (Scalar ComplexType pi)
  exp = ExpC
  log = LogC
  sin = SinC
  cos = CosC
  tan = TanC
  asin = error "TODO"
  acos = error "TODO"
  atan = error "TODO"
  sinh = SinhC
  cosh = CoshC
  tanh = TanhC
  asinh = error "TODO"
  acosh = error "TODO"
  atanh = error "TODO"

---------------------------------------------------------------------------------
-- IFunctor instance
---------------------------------------------------------------------------------

typeOfValue :: forall env t. Value '(env, t) -> TypeProxy t
typeOfValue v = case toIndex v of
  EnvType t -> t

instance IFunctor ValueF where

  type IndexProxy ValueF = EnvTypeProxy

  toIndex :: ValueF value et -> EnvTypeProxy et
  toIndex = \case
    Const (Scalar t _) -> EnvType t
    Var _ t _ -> EnvType t
    LocalLet _ _ _ _ t _ -> EnvType t

    PairV t _ _ -> EnvType t
    ProjV1 t _ -> case t of { PairType t1 _ -> EnvType t1 }
    ProjV2 t _ -> case t of { PairType _ t2 -> EnvType t2 }

    List t _ -> withKnownType t $ EnvType (ListType t)

    AddF {} -> EnvType RealType
    SubF {} -> EnvType RealType
    MulF {} -> EnvType RealType
    DivF {} -> EnvType RealType
    ModF {} -> EnvType RealType
    PowF {} -> EnvType RealType
    AbsF {} -> EnvType RealType
    NegF {} -> EnvType RealType

    ExpF {} -> EnvType RealType
    LogF {} -> EnvType RealType
    SqrtF {} -> EnvType RealType

    RoundF {} -> EnvType IntegerType
    FloorF {} -> EnvType IntegerType
    CeilingF {} -> EnvType IntegerType

    SinF {} -> EnvType RealType
    CosF {} -> EnvType RealType
    TanF {} -> EnvType RealType
    ArcsinF {} -> EnvType RealType
    ArccosF {} -> EnvType RealType
    ArctanF {} -> EnvType RealType
    Arctan2F {} -> EnvType RealType
    SinhF {} -> EnvType RealType
    CoshF {} -> EnvType RealType
    TanhF {} -> EnvType RealType
    ArcsinhF {} -> EnvType RealType
    ArccoshF {} -> EnvType RealType
    ArctanhF {} -> EnvType RealType

    AddC {} -> EnvType ComplexType
    SubC {} -> EnvType ComplexType
    MulC {} -> EnvType ComplexType
    DivC {} -> EnvType ComplexType
    PowC {} -> EnvType ComplexType
    NegC {} -> EnvType ComplexType

    AbsC {} -> EnvType RealType
    ArgC {} -> EnvType RealType
    ReC {} -> EnvType RealType
    ImC {} -> EnvType RealType
    ConjC {} -> EnvType ComplexType

    ExpC {} -> EnvType ComplexType
    LogC {} -> EnvType ComplexType
    SqrtC {} -> EnvType ComplexType

    SinC {} -> EnvType ComplexType
    CosC {} -> EnvType ComplexType
    TanC {} -> EnvType ComplexType
    SinhC {} -> EnvType ComplexType
    CoshC {} -> EnvType ComplexType
    TanhC {} -> EnvType ComplexType

    AddI {} -> EnvType IntegerType
    SubI {} -> EnvType IntegerType
    MulI {} -> EnvType IntegerType
    DivI {} -> EnvType IntegerType
    ModI {} -> EnvType IntegerType
    PowI {} -> EnvType IntegerType
    AbsI {} -> EnvType IntegerType
    NegI {} -> EnvType IntegerType

    I2R {} -> EnvType RealType
    R2C {} -> EnvType ComplexType
    C2R2 {} -> EnvType (PairType RealType RealType)
    ToText {} -> EnvType TextType

    Or {} -> EnvType BooleanType
    And {} -> EnvType BooleanType
    Not {} -> EnvType BooleanType

    ITE t _ _ _ -> EnvType t

    RGB {}       -> EnvType ColorType
    Blend {}     -> EnvType ColorType
    InvertRGB {} -> EnvType ColorType

    Eql {} -> EnvType BooleanType
    NEq {} -> EnvType BooleanType

    LTI {} -> EnvType BooleanType
    LTF {} -> EnvType BooleanType

    ConcatText {} -> EnvType TextType

    Join t _ -> withKnownType t $ EnvType (ListType t)
    Remove _ t _ _ _ -> withKnownType t $ EnvType (ListType t)
    Find _ t _ _ _ _ -> withKnownType t $ EnvType t
    Transform _ _ t _ _ _ -> withKnownType t $ EnvType (ListType t)
    Range {} -> EnvType (ListType IntegerType)
    Length {} -> EnvType IntegerType
    Index t _ _ _ -> EnvType t

  imap :: forall a b i
        . (forall j. EnvTypeProxy j -> Eval (a j) -> Eval (b j))
       -> ValueF a i
       -> ValueF b i
  imap f v0  = case lemmaEnvTy @i of
    Refl -> let { env = withEnvType (toIndex v0) (\e _ -> e); withEnv :: TypeProxy t -> EnvTypeProxy '(Env i, t); withEnv t = envTypeProxy env t } in case v0 of
      Const x -> Const x
      Var name v pf -> Var name v pf
      LocalLet name vt pf v ty e -> recallIsAbsent pf $
        let innerEnvTy = envTypeProxy (BindingProxy name vt env) ty
        in LocalLet name vt pf (f (withEnv vt) v) ty (f innerEnvTy e)

      PairV t x y ->
        let (t1, t2) = case t of { PairType tx ty -> (tx, ty) }
        in PairV t (f (withEnv t1) x) (f (withEnv t2) y)
      ProjV1 t p -> ProjV1 t (f (withEnv t) p)
      ProjV2 t p -> ProjV2 t (f (withEnv t) p)

      List t vs -> List t (map (f (withEnv t)) vs)

      AddF x y -> AddF (f (withEnv RealType) x) (f (withEnv RealType) y)

      SubF x y -> SubF (f (withEnv RealType) x) (f (withEnv RealType) y)
      MulF x y -> MulF (f (withEnv RealType) x) (f (withEnv RealType) y)
      DivF x y -> DivF (f (withEnv RealType) x) (f (withEnv RealType) y)
      ModF x y -> ModF (f (withEnv RealType) x) (f (withEnv RealType) y)
      PowF x y -> PowF (f (withEnv RealType) x) (f (withEnv RealType) y)
      AbsF x   -> AbsF (f (withEnv RealType) x)
      NegF x   -> NegF (f (withEnv RealType) x)

      ExpF x -> ExpF (f (withEnv RealType) x)
      LogF x -> LogF (f (withEnv RealType) x)
      SqrtF x -> SqrtF (f (withEnv RealType) x)

      RoundF x -> RoundF (f (withEnv RealType) x)
      FloorF x -> FloorF (f (withEnv RealType) x)
      CeilingF x -> CeilingF (f (withEnv RealType) x)

      SinF x -> SinF (f (withEnv RealType) x)
      CosF x -> CosF (f (withEnv RealType) x)
      TanF x -> TanF (f (withEnv RealType) x)
      ArcsinF x -> ArcsinF (f (withEnv RealType) x)
      ArccosF x -> ArccosF (f (withEnv RealType) x)
      ArctanF x -> ArctanF (f (withEnv RealType) x)
      Arctan2F x y -> Arctan2F (f (withEnv RealType) x) (f (withEnv RealType) y)
      SinhF x -> SinhF (f (withEnv RealType) x)
      CoshF x -> CoshF (f (withEnv RealType) x)
      TanhF x -> TanhF (f (withEnv RealType) x)
      ArcsinhF x -> ArcsinhF (f (withEnv RealType) x)
      ArccoshF x -> ArccoshF (f (withEnv RealType) x)
      ArctanhF x -> ArctanhF (f (withEnv RealType) x)

      AddC x y -> AddC (f (withEnv ComplexType) x) (f (withEnv ComplexType) y)
      SubC x y -> SubC (f (withEnv ComplexType) x) (f (withEnv ComplexType) y)
      MulC x y -> MulC (f (withEnv ComplexType) x) (f (withEnv ComplexType) y)
      DivC x y -> DivC (f (withEnv ComplexType) x) (f (withEnv ComplexType) y)
      PowC x y -> PowC (f (withEnv ComplexType) x) (f (withEnv ComplexType) y)
      NegC x   -> NegC (f (withEnv ComplexType) x)

      ExpC x -> ExpC (f (withEnv ComplexType) x)
      LogC x -> LogC (f (withEnv ComplexType) x)
      SqrtC x -> SqrtC (f (withEnv ComplexType) x)

      SinC x -> SinC (f (withEnv ComplexType) x)
      CosC x -> CosC (f (withEnv ComplexType) x)
      TanC x -> TanC (f (withEnv ComplexType) x)
      SinhC x -> SinhC (f (withEnv ComplexType) x)
      CoshC x -> CoshC (f (withEnv ComplexType) x)
      TanhC x -> TanhC (f (withEnv ComplexType) x)

      AbsC x -> AbsC (f (withEnv ComplexType) x)
      ArgC x -> ArgC (f (withEnv ComplexType) x)
      ReC x -> ReC (f (withEnv ComplexType) x)
      ImC x -> ImC (f (withEnv ComplexType) x)
      ConjC x -> ConjC (f (withEnv ComplexType) x)

      AddI x y -> AddI (f (withEnv IntegerType) x) (f (withEnv IntegerType) y)
      SubI x y -> SubI (f (withEnv IntegerType) x) (f (withEnv IntegerType) y)
      MulI x y -> MulI (f (withEnv IntegerType) x) (f (withEnv IntegerType) y)
      DivI x y -> DivI (f (withEnv IntegerType) x) (f (withEnv IntegerType) y)
      ModI x y -> ModI (f (withEnv IntegerType) x) (f (withEnv IntegerType) y)
      PowI x y -> PowI (f (withEnv IntegerType) x) (f (withEnv IntegerType) y)
      AbsI x   -> AbsI (f (withEnv IntegerType) x)
      NegI x   -> NegI (f (withEnv IntegerType) x)

      I2R x -> I2R (f (withEnv IntegerType) x)
      R2C x -> R2C (f (withEnv RealType) x)
      C2R2 x -> C2R2 (f (withEnv ComplexType) x)
      ToText t x -> ToText t (f (withEnv t) x)

      Or  x y -> Or  (f (withEnv BooleanType) x) (f (withEnv BooleanType) y)
      And x y -> And (f (withEnv BooleanType) x) (f (withEnv BooleanType) y)
      Not x -> Not (f (withEnv BooleanType) x)

      ITE t b yes no -> ITE t (f (withEnv BooleanType) b) (f (withEnv t) yes) (f (withEnv t) no)

      RGB r g b -> RGB (f (withEnv RealType) r) (f (withEnv RealType) g) (f (withEnv RealType) b)
      Blend s c1 c2 -> Blend (f (withEnv RealType) s) (f (withEnv ColorType) c1) (f (withEnv ColorType) c2)
      InvertRGB c -> InvertRGB (f (withEnv ColorType) c)

      Eql t x y -> Eql t (f (withEnv t) x) (f (withEnv t) y)
      NEq t x y -> NEq t (f (withEnv t) x) (f (withEnv t) y)

      LTI x y -> LTI (f (withEnv IntegerType) x) (f (withEnv IntegerType) y)
      LTF x y -> LTF (f (withEnv RealType)    x) (f (withEnv RealType)    y)

      ConcatText xs -> ConcatText (map (f (withEnv TextType)) xs)

      Join t xs -> withKnownType t $ Join t (map (f (withEnv (ListType t))) xs)
      Remove n t pf lst test -> recallIsAbsent pf $ withKnownType t $
        let innerEnv = envTypeProxy (BindingProxy n t env)
        in Remove n t pf (f (withEnv (ListType t)) lst) (f (innerEnv BooleanType) test)
      Find n t pf lst test x -> recallIsAbsent pf $ withKnownType t $
        let innerEnv = envTypeProxy (BindingProxy n t env)
        in Find n t pf (f (withEnv (ListType t)) lst) (f (innerEnv BooleanType) test) (f (withEnv t) x)
      Transform n t1 t2 pf lst m -> recallIsAbsent pf $ withKnownType t1 $ withKnownType t2 $
        let innerEnv = envTypeProxy (BindingProxy n t1 env)
        in Transform n t1 t2 pf (f (withEnv (ListType t1)) lst) (f (innerEnv t2) m)
      Range lo hi -> Range (f (withEnv IntegerType) lo) (f (withEnv IntegerType) hi)
      Length t xs -> withKnownType t $ Length t (f (withEnv (ListType t)) xs)
      Index t cyc xs i -> withKnownType t $
        Index t cyc (f (withEnv (ListType t)) xs) (f (withEnv IntegerType) i)

---------------------------------------------------------------------------------
-- Indexed traversable instance for values
---------------------------------------------------------------------------------

instance ITraversable ValueF  where
  isequence = \case
    Const x -> pure (Const x)
    Var name v pf -> pure (Var name v pf)
    LocalLet name vty pf v ty e ->
      LocalLet name vty pf <$> v <*> pure ty <*> e

    PairV t mx my -> PairV t <$> mx <*> my
    ProjV1 t mp   -> ProjV1 t <$> mp
    ProjV2 t mp   -> ProjV2 t <$> mp

    List t mvs -> List t <$> sequenceA mvs

    AddF mx my -> AddF <$> mx <*> my
    SubF mx my -> SubF <$> mx <*> my
    MulF mx my -> MulF <$> mx <*> my
    DivF mx my -> DivF <$> mx <*> my
    ModF mx my -> ModF <$> mx <*> my
    PowF mx my -> PowF <$> mx <*> my
    AbsF mx    -> AbsF <$> mx
    NegF mx    -> NegF <$> mx

    RoundF   mx -> RoundF   <$> mx
    FloorF   mx -> FloorF   <$> mx
    CeilingF mx -> CeilingF <$> mx

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
    C2R2 mx -> C2R2 <$> mx
    ToText t mx -> ToText t <$> mx

    Or  mx my -> Or  <$> mx <*> my
    And mx my -> And <$> mx <*> my
    Not mx    -> Not <$> mx

    ITE t mb myes mno -> ITE t <$> mb <*> myes <*> mno

    RGB mr mg mb -> RGB <$> mr <*> mg <*> mb
    Blend ms mc1 mc2 -> Blend <$> ms <*> mc1 <*> mc2
    InvertRGB mc -> InvertRGB <$> mc

    Eql t mx my -> Eql t <$> mx <*> my
    NEq t mx my -> NEq t <$> mx <*> my

    LTI mx my -> LTI <$> mx <*> my
    LTF mx my -> LTF <$> mx <*> my

    ConcatText mxs -> ConcatText <$> sequenceA mxs

    Join t mxs -> Join t <$> sequenceA mxs
    Remove n t pf mlst mtest -> Remove n t pf <$> mlst <*> mtest
    Find n t pf mlst mtest md -> Find n t pf <$> mlst <*> mtest <*> md
    Transform n t1 t2 pf mlst mf -> Transform n t1 t2 pf <$> mlst <*> mf
    Range mlo mhi -> Range <$> mlo <*> mhi
    Length t mlst -> Length t <$> mlst
    Index t cyc mlst mi -> Index t cyc <$> mlst <*> mi

instance Show (Value et) where show = pprint

data PrecString :: (Environment, FSType) -> Exp Type
type instance Eval (PrecString et) = String -- (Int, String)

pprint :: Value et -> String
pprint = indexedFold @PrecString @ValueF go
 where
  go :: forall et'. ValueF PrecString et' -> String
  go = \case
    Const (Scalar t c) -> case t of
      BooleanType -> show c
      IntegerType -> show c
      RealType    -> show c
      ComplexType -> show c
      RationalType -> show c
      ColorType -> show c
      VoidType  -> show c
      ImageType -> show c
      TextType  -> show c
      PairType (t1 :: TypeProxy t1) (t2 :: TypeProxy t2) ->
        let (x,y) = c
        in concat [ "(", go @'(Env et', t1) (Const (Scalar t1 x)), ", "
                  , go @'(Env et', t2) (Const (Scalar t2 y)), ")"]
      ListType (it :: TypeProxy it) ->
        "[" <> intercalate "; " (map (go @'(Env et', it) . Const . Scalar it) c) <> "]"
    Var name _ _ -> symbolVal name
    LocalLet name _ _ v _ e -> concat
      ["let ", symbolVal name, " = ", v, " in ", e]
    PairV _ x y  -> concat ["(", x, ", ", y, ")"]
    ProjV1 _ p   -> concat ["(1st ", p, ")"]
    ProjV2 _ p   -> concat ["(2nd ", p, ")"]
    List _ xs -> "[" ++ intercalate "; " xs ++ "]"
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
    C2R2 x   -> x ++ ":R^2"
    ToText _ x -> x ++ ":Text"
    Or x y   -> binop "or" x y
    And x y  -> binop "and" x y
    Not x    -> "!" ++ x
    Eql _ x y -> binop "=" x y
    NEq _ x y -> binop "≠" x y
    LTI x y  -> binop "<" x y
    LTF x y  -> binop "<" x y
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
    RoundF x -> fun "round" [x]
    FloorF x -> fun "floor" [x]
    CeilingF x -> fun "ceil" [x]
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
    ConcatText xs -> fun "text" xs
    Join _ xs -> fun "join" xs
    Remove n _ _ lst p -> concat ["remove(", lst, ", ", symbolVal n, " ⟼ ", p, ")"]
    Find n _ _ lst p d -> concat ["find(", lst, ", ", symbolVal n, " ⟼ ", p, ", ", d, ")"]
    Transform n _ _ _ lst f -> concat ["transform(", lst, ", ", symbolVal n, " ⟼ ", f, ")"]
    Range lo hi -> fun "range" [lo, hi]
    Length _ x -> concat ["|", x, "|"]
    Index _ cyc lst i -> concat ["(", lst, if cyc then "@@" else "@", i, ")"]

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
--     get @"foo" IntegerType
--
-- vs
--
--     Get (Proxy :: Proxy "foo") IntegerType
--
getValue :: forall name env ty
          . ( Required name env ~ ty, NotPresent name (env `Without` name)
          , KnownSymbol name, KnownEnvironment env)
         => TypeProxy ty
         -> Value '(env, ty)
getValue ty = Var (Proxy @name) ty bindingEvidence
