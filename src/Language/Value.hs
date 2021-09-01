{-# language AllowAmbiguousTypes #-}

module Language.Value
  ( module Language.Type
  , type Value
  , ValueF(..)
  , Proxy(..)
  , get
  ) where

import Data.Proxy (Proxy(..))
import Fcf
import Language.Type
import Data.Indexed.Functor
import GHC.TypeLits

{-
data Value (env :: [(Symbol, Type)]) (t :: Type) where

  -- Constants and variables
  Const :: forall ty env. Scalar ty -> Value env ty
  Var :: forall name ty env
       . Required name env ~ ty
      => Proxy name
      -> Value env ty

  -- Product types and projections
  PairV :: forall t1 t2 env. Value env t1 -> Value env t2 -> Value env ('Pair t1 t2)
  ProjV1 :: forall t1 t2 env. Value env ('Pair t1 t2) -> Value env t1
  ProjV2 :: forall t1 t2 env. Value env ('Pair t1 t2) -> Value env t2

  -- Floating-point arithmetic
  AddF :: forall env. Value env 'RealT -> Value env 'RealT -> Value env 'RealT
  SubF :: forall env. Value env 'RealT -> Value env 'RealT -> Value env 'RealT
  MulF :: forall env. Value env 'RealT -> Value env 'RealT -> Value env 'RealT
  DivF :: forall env. Value env 'RealT -> Value env 'RealT -> Value env 'RealT
  ModF :: forall env. Value env 'RealT -> Value env 'RealT -> Value env 'RealT
  PowF :: forall env. Value env 'RealT -> Value env 'IntegerT -> Value env 'RealT

  -- Exponential and logarithmic functions
  ExpF :: forall env. Value env 'RealT -> Value env 'RealT
  LogF :: forall env. Value env 'RealT -> Value env 'RealT

  -- Trigonometric functions
  SinF :: forall env. Value env 'RealT -> Value env 'RealT
  CosF :: forall env. Value env 'RealT -> Value env 'RealT
  TanF :: forall env. Value env 'RealT -> Value env 'RealT
  ArcsinF :: forall env. Value env 'RealT -> Value env 'RealT
  ArccosF :: forall env. Value env 'RealT -> Value env 'RealT
  ArctanF :: forall env. Value env 'RealT -> Value env 'RealT
  Arctan2F :: forall env. Value env 'RealT -> Value env 'RealT -> Value env 'RealT

  -- Integer arithmetic
  AddI :: forall env. Value env 'IntegerT -> Value env 'IntegerT -> Value env 'IntegerT
  SubI :: forall env. Value env 'IntegerT -> Value env 'IntegerT -> Value env 'IntegerT
  MulI :: forall env. Value env 'IntegerT -> Value env 'IntegerT -> Value env 'IntegerT
  DivI :: forall env. Value env 'IntegerT -> Value env 'IntegerT -> Value env 'IntegerT
  ModI :: forall env. Value env 'IntegerT -> Value env 'IntegerT -> Value env 'IntegerT
  PowI :: forall env. Value env 'IntegerT -> Value env 'IntegerT -> Value env 'IntegerT

  -- Boolean operations
  Or  :: forall env. Value env 'BooleanT -> Value env 'BooleanT -> Value env 'BooleanT
  And :: forall env. Value env 'BooleanT -> Value env 'BooleanT -> Value env 'BooleanT
  Not :: forall env. Value env 'BooleanT -> Value env 'BooleanT

  -- If/then/else expression
  ITE :: forall env t
       . Value env 'BooleanT
      -> Value env t
      -> Value env t
      -> Value env t

  -- Equality tests
  Eql :: forall env t. Value env t -> Value env t -> Value env 'BooleanT
  NEq :: forall env t. Value env t -> Value env t -> Value env 'BooleanT

  -- Scalar comparisons
  LEI :: forall env. Value env 'IntegerT -> Value env 'IntegerT -> Value env 'BooleanT
  LEF :: forall env. Value env 'RealT -> Value env 'RealT -> Value env 'BooleanT
  GEI :: forall env. Value env 'IntegerT -> Value env 'IntegerT -> Value env 'BooleanT
  GEF :: forall env. Value env 'RealT -> Value env 'RealT -> Value env 'BooleanT
  LTI :: forall env. Value env 'IntegerT -> Value env 'IntegerT -> Value env 'BooleanT
  LTF :: forall env. Value env 'RealT -> Value env 'RealT -> Value env 'BooleanT
  GTI :: forall env. Value env 'IntegerT -> Value env 'IntegerT -> Value env 'BooleanT
  GTF :: forall env. Value env 'RealT -> Value env 'RealT -> Value env 'BooleanT
-}

---------------------------------------------------------------------------------
-- Value
---------------------------------------------------------------------------------

type Value env = Fix (ValueF env)

data ValueF (env :: [(Symbol, Type)]) (value :: Type -> Exp *) (t :: Type) where

  -- Constants and variables
  Const :: forall ty env value. Scalar ty -> ValueF env value ty
  Var :: forall name ty env value
       . (Required name env ~ ty, KnownSymbol name)
      => Proxy name
      -> ScalarProxy ty
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
  PowF :: forall env value. Eval (value 'RealT) -> Eval (value 'IntegerT) -> ValueF env value 'RealT
  AbsF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT

  -- Exponential and logarithmic functions
  ExpF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  LogF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT

  -- Trigonometric functions
  SinF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  CosF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  TanF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  ArcsinF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  ArccosF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  ArctanF :: forall env value. Eval (value 'RealT) -> ValueF env value 'RealT
  Arctan2F :: forall env value. Eval (value 'RealT) -> Eval (value 'RealT) -> ValueF env value 'RealT

  -- Integer arithmetic
  AddI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'IntegerT
  SubI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'IntegerT
  MulI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'IntegerT
  DivI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'IntegerT
  ModI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'IntegerT
  PowI :: forall env value. Eval (value 'IntegerT) -> Eval (value 'IntegerT) -> ValueF env value 'IntegerT
  AbsI :: forall env value. Eval (value 'IntegerT) -> ValueF env value 'IntegerT

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

---------------------------------------------------------------------------------
-- IFunctor instance
---------------------------------------------------------------------------------

instance IFunctor (ValueF env) where

  type IndexProxy (ValueF env) = ScalarProxy

  toIndex = \case
    Const (Scalar t _) -> t
    Var _ t -> t

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

    ExpF {} -> RealProxy
    LogF {} -> RealProxy

    SinF {} -> RealProxy
    CosF {} -> RealProxy
    TanF {} -> RealProxy
    ArcsinF {} -> RealProxy
    ArccosF {} -> RealProxy
    ArctanF {} -> RealProxy
    Arctan2F {} -> RealProxy

    AddI {} -> IntegerProxy
    SubI {} -> IntegerProxy
    MulI {} -> IntegerProxy
    DivI {} -> IntegerProxy
    ModI {} -> IntegerProxy
    PowI {} -> IntegerProxy
    AbsI {} -> IntegerProxy

    Or {} -> BooleanProxy
    And {} -> BooleanProxy
    Not {} -> BooleanProxy

    ITE t _ _ _ -> t

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
    Var name v -> Var name v

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
    PowF x y -> PowF (f RealProxy x) (f IntegerProxy y)
    AbsF x   -> AbsF (f RealProxy x)

    ExpF x -> ExpF (f RealProxy x)
    LogF x -> LogF (f RealProxy x)

    SinF x -> SinF (f RealProxy x)
    CosF x -> CosF (f RealProxy x)
    TanF x -> TanF (f RealProxy x)
    ArcsinF x -> ArcsinF (f RealProxy x)
    ArccosF x -> ArccosF (f RealProxy x)
    ArctanF x -> ArctanF (f RealProxy x)
    Arctan2F x y -> Arctan2F (f RealProxy x) (f RealProxy y)

    AddI x y -> AddI (f IntegerProxy x) (f IntegerProxy y)
    SubI x y -> SubI (f IntegerProxy x) (f IntegerProxy y)
    MulI x y -> MulI (f IntegerProxy x) (f IntegerProxy y)
    DivI x y -> DivI (f IntegerProxy x) (f IntegerProxy y)
    ModI x y -> ModI (f IntegerProxy x) (f IntegerProxy y)
    PowI x y -> PowI (f IntegerProxy x) (f IntegerProxy y)
    AbsI x   -> AbsI (f IntegerProxy x)

    Or  x y -> Or  (f BooleanProxy x) (f BooleanProxy y)
    And x y -> And (f BooleanProxy x) (f BooleanProxy y)
    Not x -> Not (f BooleanProxy x)

    ITE t b yes no -> ITE t (f BooleanProxy b) (f t yes) (f t no)

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
    Var name v -> pure (Var name v)

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

    ExpF mx -> ExpF <$> mx
    LogF mx -> LogF <$> mx

    SinF mx -> SinF <$> mx
    CosF mx -> CosF <$> mx
    TanF mx -> TanF <$> mx
    ArcsinF mx -> ArcsinF <$> mx
    ArccosF mx -> ArccosF <$> mx
    ArctanF mx -> ArctanF <$> mx
    Arctan2F mx my -> Arctan2F <$> mx <*> my

    AddI mx my -> AddI <$> mx <*> my
    SubI mx my -> SubI <$> mx <*> my
    MulI mx my -> MulI <$> mx <*> my
    DivI mx my -> DivI <$> mx <*> my
    ModI mx my -> ModI <$> mx <*> my
    PowI mx my -> PowI <$> mx <*> my
    AbsI mx    -> AbsI <$> mx

    Or  mx my -> Or  <$> mx <*> my
    And mx my -> And <$> mx <*> my
    Not mx    -> Not <$> mx

    ITE t mb myes mno -> ITE t <$> mb <*> myes <*> mno

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
     . (Required name env ~ ty, KnownSymbol name)
    => ScalarProxy ty
    -> Value env ty
get ty = Fix (Var (Proxy @name) ty)
