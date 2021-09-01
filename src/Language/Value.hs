{-# language AllowAmbiguousTypes #-}

module Language.Value
  ( module Language.Type
  , Value(..)
  , Proxy(..)
  , get
  ) where

import Data.Proxy (Proxy(..))

import Language.Type

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


-- | Get the value of a variable.
-- This is the same as using the 'Get' constructor directly,
-- except that it saves you the syntactic noise of using a
-- 'Proxy', by using type applications instead.
--
--     get @"foo"
--
-- vs
--
--     Get (Proxy :: Proxy "foo")
--
get :: forall name env ty
     . Required name env ~ ty
    => Value env ty
get = Var (Proxy @name)
