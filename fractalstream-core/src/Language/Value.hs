{-# language AllowAmbiguousTypes #-}

module Language.Value
  ( module Language.Type
  , module Language.Environment
  , type Value
  , ValueF(..)
  , Proxy(..)
  , typeOfValue
  , get
  ) where

import Data.Proxy (Proxy(..))
import Fcf
import GHC.TypeLits
import Data.Ratio

import Data.Indexed.Functor
import Language.Type
import Language.Environment

---------------------------------------------------------------------------------
-- Value
---------------------------------------------------------------------------------

type Value env = Fix (ValueF env)

data ValueF (env :: Environment) (value :: Type -> Exp *) (t :: Type) where

  -- Constants and variables
  Const :: forall ty env value. Scalar ty -> ValueF env value ty
  Var :: forall name ty env value
       . (KnownSymbol name)
      => Proxy name
      -> ScalarProxy ty
      -> NameIsPresent name ty env
      -> ValueF env value ty

  -- Product types and projections
  PairV :: forall t1 t2 env value. ScalarProxy ('Pair t1 t2) -> Eval (value t1) -> Eval (value t2) -> ValueF env value ('Pair t1 t2)
  ProjV1 :: forall t1 t2 env value. ScalarProxy ('Pair t1 t2) -> Eval (value ('Pair t1 t2)) -> ValueF env value t1
  ProjV2 :: forall t1 t2 env value. ScalarProxy ('Pair t1 t2) -> Eval (value ('Pair t1 t2)) -> ValueF env value t2

  -- Floating-point arithmetic
  AddF :: forall env value. Eval (value 'RealT) -> Eval (value 'RealT) -> ValueF env value 'RealT
  SubF :: forall env value. Eval (value 'RealT) -> Eval (value 'RealT) -> ValueF env value 'RealT
  MulF :: forall env value. Eval (value 'RealT) -> Eval (value 'RealT) -> ValueF env value 'RealT
  DivF :: forall env value. Eval (value 'RealT) -> Eval (value 'RealT) -> ValueF env value 'RealT
  ModF :: forall env value. Eval (value 'RealT) -> Eval (value 'RealT) -> ValueF env value 'RealT
  PowF :: forall env value. Eval (value 'RealT) -> Eval (value 'RealT) -> ValueF env value 'RealT
  AbsF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  NegF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT

  -- Exponential and logarithmic functions
  ExpF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  LogF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  SqrtF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT

  -- Trigonometric functions
  SinF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  CosF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  TanF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  SinhF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  CoshF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  TanhF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  ArcsinF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  ArccosF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  ArctanF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  Arctan2F :: forall env value. Eval (value 'RealT) -> Eval (value 'RealT) -> ValueF env value 'RealT
  ArcsinhF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  ArccoshF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  ArctanhF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT

  -- Complex arithmetic
  AddC :: forall env value. Eval (value 'ComplexT) -> Eval (value 'ComplexT) -> ValueF env value 'ComplexT
  SubC :: forall env value. Eval (value 'ComplexT) -> Eval (value 'ComplexT) -> ValueF env value 'ComplexT
  MulC :: forall env value. Eval (value 'ComplexT) -> Eval (value 'ComplexT) -> ValueF env value 'ComplexT
  DivC :: forall env value. Eval (value 'ComplexT) -> Eval (value 'ComplexT) -> ValueF env value 'ComplexT
  PowC :: forall env value. Eval (value 'ComplexT) -> Eval (value 'ComplexT) -> ValueF env value 'ComplexT
  NegC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'ComplexT

  -- Complex-only functions
  AbsC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'RealT
  ArgC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'RealT
  ReC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'RealT
  ImC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'RealT
  ConjC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'ComplexT

  -- Complex exponential and logarithmic functions
  ExpC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'ComplexT
  LogC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'ComplexT
  SqrtC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'ComplexT

  -- Complex trigonometric functions
  SinC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'ComplexT
  CosC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'ComplexT
  TanC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'ComplexT
  SinhC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'ComplexT
  CoshC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'ComplexT
  TanhC :: forall env value. Eval (value 'ComplexT) -> ValueF env value 'ComplexT

  -- Integer arithmetic
  AddI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'IntegerT
  SubI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'IntegerT
  MulI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'IntegerT
  DivI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'IntegerT
  ModI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'IntegerT
  PowI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'IntegerT
  AbsI :: forall env value. Eval (value 'IntegerT) -> ValueF env value 'IntegerT
  NegI :: forall env value. Eval (value 'IntegerT) -> ValueF env value 'IntegerT

  -- Conversion
  I2R :: forall env value. Eval (value 'IntegerT) -> ValueF env value 'RealT
  R2C :: forall env value. Eval (value 'RealT) -> ValueF env value 'ComplexT

  -- Boolean operations
  Or  :: forall env value. Eval (value 'BooleanT) -> Eval (value 'BooleanT) -> ValueF env value 'BooleanT
  And :: forall env value. Eval (value 'BooleanT) -> Eval (value 'BooleanT) -> ValueF env value 'BooleanT
  Not :: forall env value. Eval (value 'BooleanT) -> ValueF env value 'BooleanT

  -- If/then/else expression
  ITE :: forall env t value
       . ScalarProxy t
      -> Eval (value 'BooleanT)
      -> Eval (value t)
      -> Eval (value t)
      -> ValueF env value t

  -- Color operations
  RGB :: forall env value
       . Eval (value 'RealT)
      -> Eval (value 'RealT)
      -> Eval (value 'RealT)
      -> ValueF env value 'ColorT

  Blend :: forall env value
         . Eval (value 'RealT)
        -> Eval (value 'ColorT)
        -> Eval (value 'ColorT)
        -> ValueF env value 'ColorT

  InvertRGB :: forall env value
             . Eval (value 'ColorT)
            -> ValueF env value 'ColorT

  -- Equality tests
  Eql :: forall env t value. ScalarProxy t -> Eval (value t) -> Eval (value t) -> ValueF env value 'BooleanT
  NEq :: forall env t value. ScalarProxy t -> Eval (value t) -> Eval (value t) -> ValueF env value 'BooleanT

  -- Scalar comparisons
  LEI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'BooleanT
  LEF :: forall env value. Eval (value 'RealT) -> Eval (value 'RealT) -> ValueF env value 'BooleanT
  GEI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'BooleanT
  GEF :: forall env value. Eval (value 'RealT) -> Eval (value 'RealT) -> ValueF env value 'BooleanT
  LTI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'BooleanT
  LTF :: forall env value. Eval (value 'RealT) -> Eval (value 'RealT) -> ValueF env value 'BooleanT
  GTI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'BooleanT
  GTF :: forall env value. Eval (value 'RealT) -> Eval (value 'RealT) -> ValueF env value 'BooleanT

fix2 :: (Value env t -> Value env t -> ValueF env (Pure1 (Value env)) t)
     ->  Value env t -> Value env t -> Value env t
fix2 (#) x y = Fix (x # y)

instance Num (Value env 'IntegerT) where
  (+) = fix2 AddI
  (-) = fix2 SubI
  (*) = fix2 MulI
  abs = Fix . AbsI
  negate = Fix . NegI
  fromInteger = Fix . Const . Scalar IntegerProxy . fromInteger
  signum = error "TODO"

instance Num (Value env 'RealT) where
  (+) = fix2 AddF
  (-) = fix2 SubF
  (*) = fix2 MulF
  abs = Fix . AbsF
  negate = Fix . NegF
  fromInteger = Fix . Const . Scalar RealProxy . fromInteger
  signum = error "TODO"

instance Fractional (Value env 'RealT) where
  (/) = fix2 DivF
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)

instance Floating (Value env 'RealT) where
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

instance Num (Value env 'ComplexT) where
  (+) = fix2 AddC
  (-) = fix2 SubC
  (*) = fix2 MulC
  abs = Fix . R2C . Fix . AbsC
  negate = Fix . NegC
  fromInteger = Fix . Const . Scalar ComplexProxy . fromInteger
  signum = error "TODO"

instance Fractional (Value env 'ComplexT) where
  (/) = fix2 DivC
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)

instance Floating (Value env 'ComplexT) where
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

typeOfValue :: forall env t. Value env t -> ScalarProxy t
typeOfValue = toIndex . unrollIx @_ @(Value env) @(ValueF env)

instance IFunctor (ValueF env) where

  type IndexProxy (ValueF env) = ScalarProxy

  toIndex = \case
    Const (Scalar t _) -> t
    Var _ t _ -> t

    PairV t _ _ -> t
    ProjV1 t _ -> case t of { PairProxy t1 _ -> t1 }
    ProjV2 t _ -> case t of { PairProxy _ t2 -> t2 }

    AddF {} -> RealProxy
    SubF {} -> RealProxy
    MulF {} -> RealProxy
    DivF {} -> RealProxy
    ModF {} -> RealProxy
    PowF {} -> RealProxy
    AbsF {} -> RealProxy
    NegF {} -> RealProxy

    ExpF {} -> RealProxy
    LogF {} -> RealProxy
    SqrtF {} -> RealProxy

    SinF {} -> RealProxy
    CosF {} -> RealProxy
    TanF {} -> RealProxy
    ArcsinF {} -> RealProxy
    ArccosF {} -> RealProxy
    ArctanF {} -> RealProxy
    Arctan2F {} -> RealProxy
    SinhF {} -> RealProxy
    CoshF {} -> RealProxy
    TanhF {} -> RealProxy
    ArcsinhF {} -> RealProxy
    ArccoshF {} -> RealProxy
    ArctanhF {} -> RealProxy

    AddC {} -> ComplexProxy
    SubC {} -> ComplexProxy
    MulC {} -> ComplexProxy
    DivC {} -> ComplexProxy
    PowC {} -> ComplexProxy
    NegC {} -> ComplexProxy

    AbsC {} -> RealProxy
    ArgC {} -> RealProxy
    ReC {} -> RealProxy
    ImC {} -> RealProxy
    ConjC {} -> ComplexProxy

    ExpC {} -> ComplexProxy
    LogC {} -> ComplexProxy
    SqrtC {} -> ComplexProxy

    SinC {} -> ComplexProxy
    CosC {} -> ComplexProxy
    TanC {} -> ComplexProxy
    SinhC {} -> ComplexProxy
    CoshC {} -> ComplexProxy
    TanhC {} -> ComplexProxy

    AddI {} -> IntegerProxy
    SubI {} -> IntegerProxy
    MulI {} -> IntegerProxy
    DivI {} -> IntegerProxy
    ModI {} -> IntegerProxy
    PowI {} -> IntegerProxy
    AbsI {} -> IntegerProxy
    NegI {} -> IntegerProxy

    I2R {} -> RealProxy
    R2C {} -> ComplexProxy

    Or {} -> BooleanProxy
    And {} -> BooleanProxy
    Not {} -> BooleanProxy

    ITE t _ _ _ -> t

    RGB {}       -> ColorProxy
    Blend {}     -> ColorProxy
    InvertRGB {} -> ColorProxy

    Eql {} -> BooleanProxy
    NEq {} -> BooleanProxy

    LEI {} -> BooleanProxy
    LEF {} -> BooleanProxy
    GEI {} -> BooleanProxy
    GEF {} -> BooleanProxy
    LTI {} -> BooleanProxy
    LTF {} -> BooleanProxy
    GTI {} -> BooleanProxy
    GTF {} -> BooleanProxy

  imap f = \case
    Const x -> Const x
    Var name v pf -> Var name v pf

    PairV t x y ->
      let (t1, t2) = case t of { PairProxy tx ty -> (tx, ty) }
      in PairV t (f t1 x) (f t2 y)
    ProjV1 t p -> ProjV1 t (f t p)
    ProjV2 t p -> ProjV2 t (f t p)

    AddF x y -> AddF (f RealProxy x) (f RealProxy y)
    SubF x y -> SubF (f RealProxy x) (f RealProxy y)
    MulF x y -> MulF (f RealProxy x) (f RealProxy y)
    DivF x y -> DivF (f RealProxy x) (f RealProxy y)
    ModF x y -> ModF (f RealProxy x) (f RealProxy y)
    PowF x y -> PowF (f RealProxy x) (f RealProxy y)
    AbsF x   -> AbsF (f RealProxy x)
    NegF x   -> NegF (f RealProxy x)

    ExpF x -> ExpF (f RealProxy x)
    LogF x -> LogF (f RealProxy x)
    SqrtF x -> SqrtF (f RealProxy x)

    SinF x -> SinF (f RealProxy x)
    CosF x -> CosF (f RealProxy x)
    TanF x -> TanF (f RealProxy x)
    ArcsinF x -> ArcsinF (f RealProxy x)
    ArccosF x -> ArccosF (f RealProxy x)
    ArctanF x -> ArctanF (f RealProxy x)
    Arctan2F x y -> Arctan2F (f RealProxy x) (f RealProxy y)
    SinhF x -> SinhF (f RealProxy x)
    CoshF x -> CoshF (f RealProxy x)
    TanhF x -> TanhF (f RealProxy x)
    ArcsinhF x -> ArcsinhF (f RealProxy x)
    ArccoshF x -> ArccoshF (f RealProxy x)
    ArctanhF x -> ArctanhF (f RealProxy x)

    AddC x y -> AddC (f ComplexProxy x) (f ComplexProxy y)
    SubC x y -> SubC (f ComplexProxy x) (f ComplexProxy y)
    MulC x y -> MulC (f ComplexProxy x) (f ComplexProxy y)
    DivC x y -> DivC (f ComplexProxy x) (f ComplexProxy y)
    PowC x y -> PowC (f ComplexProxy x) (f ComplexProxy y)
    NegC x   -> NegC (f ComplexProxy x)

    ExpC x -> ExpC (f ComplexProxy x)
    LogC x -> LogC (f ComplexProxy x)
    SqrtC x -> SqrtC (f ComplexProxy x)

    SinC x -> SinC (f ComplexProxy x)
    CosC x -> CosC (f ComplexProxy x)
    TanC x -> TanC (f ComplexProxy x)
    SinhC x -> SinhC (f ComplexProxy x)
    CoshC x -> CoshC (f ComplexProxy x)
    TanhC x -> TanhC (f ComplexProxy x)

    AbsC x -> AbsC (f ComplexProxy x)
    ArgC x -> ArgC (f ComplexProxy x)
    ReC x -> ReC (f ComplexProxy x)
    ImC x -> ImC (f ComplexProxy x)
    ConjC x -> ConjC (f ComplexProxy x)

    AddI x y -> AddI (f IntegerProxy x) (f IntegerProxy y)
    SubI x y -> SubI (f IntegerProxy x) (f IntegerProxy y)
    MulI x y -> MulI (f IntegerProxy x) (f IntegerProxy y)
    DivI x y -> DivI (f IntegerProxy x) (f IntegerProxy y)
    ModI x y -> ModI (f IntegerProxy x) (f IntegerProxy y)
    PowI x y -> PowI (f IntegerProxy x) (f IntegerProxy y)
    AbsI x   -> AbsI (f IntegerProxy x)
    NegI x   -> NegI (f IntegerProxy x)

    I2R x -> I2R (f IntegerProxy x)
    R2C x -> R2C (f RealProxy x)

    Or  x y -> Or  (f BooleanProxy x) (f BooleanProxy y)
    And x y -> And (f BooleanProxy x) (f BooleanProxy y)
    Not x -> Not (f BooleanProxy x)

    ITE t b yes no -> ITE t (f BooleanProxy b) (f t yes) (f t no)

    RGB r g b -> RGB (f RealProxy r) (f RealProxy g) (f RealProxy b)
    Blend s c1 c2 -> Blend (f RealProxy s) (f ColorProxy c1) (f ColorProxy c2)
    InvertRGB c -> InvertRGB (f ColorProxy c)

    Eql t x y -> Eql t (f t x) (f t y)
    NEq t x y -> NEq t (f t x) (f t y)

    LEI x y -> LEI (f IntegerProxy x) (f IntegerProxy y)
    LEF x y -> LEF (f RealProxy    x) (f RealProxy    y)
    GEI x y -> GEI (f IntegerProxy x) (f IntegerProxy y)
    GEF x y -> GEF (f RealProxy    x) (f RealProxy    y)
    LTI x y -> LTI (f IntegerProxy x) (f IntegerProxy y)
    LTF x y -> LTF (f RealProxy    x) (f RealProxy    y)
    GTI x y -> GTI (f IntegerProxy x) (f IntegerProxy y)
    GTF x y -> GTF (f RealProxy    x) (f RealProxy    y)

---------------------------------------------------------------------------------
-- Indexed traversable instance for values
---------------------------------------------------------------------------------

instance ITraversable (ValueF env) where
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

instance Show (Value env t) where show _ = "<value>"

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
     . (Required name env ~ ty, NotPresent name (env `Without` name), KnownSymbol name)
    => ScalarProxy ty
    -> Value env ty
get ty = Fix (Var (Proxy @name) ty bindingEvidence)
