{-# language AllowAmbiguousTypes, UndecidableInstances #-}

module Language.Value.Derivative
  (derivative) where

import FractalStream.Prelude

import Data.Indexed.Functor
import Language.Value

-- Computes the partial derivative of a Value et with respect to z
derivative :: forall et. String -> Value et -> Value et
derivative z = indexedFoldWithOriginal derivativeRules
  where
    derivativeRules :: forall s. ValueF (FIX ValueF :*: FIX ValueF) s -> Value s
    derivativeRules = \case

      -- | Constant case
      Const (Scalar ty _) -> case ty of
        ComplexType -> 0
        RealType    -> 0
        IntegerType -> 0
        _           -> error "not differentiable"

      -- | Other variables are assumed to be constant with respect to z 
      Var name ty _ -> case ty of
        ComplexType -> if symbolVal name == z
                       then Const (Scalar ComplexType 1)
                       else Const (Scalar ComplexType 0)
        RealType    -> if symbolVal name == z
                       then Const (Scalar RealType 1)
                       else Const (Scalar RealType 0)
        _           -> error "can only differentiate real and complex numbers"

      -- | Basic algebra

      AddF (_, dx) (_, dy) -> dx + dy
      SubF (_, dx) (_, dy) -> dx - dy
      MulF (x, dx) (y, dy) -> y * dx + x * dy
      DivF (x, dx) (y, dy) -> (y * dx - x * dy) / (y * y)
      PowF (x, dx) (n, _)  -> n * x ** (n-1) * dx
      NegF (_, dx)         -> - dx

      AddC (_, dx) (_, dy) -> dx + dy
      SubC (_, dx) (_, dy) -> dx - dy
      MulC (x, dx) (y, dy) -> y * dx + x * dy
      DivC (x, dx) (y, dy) -> (y * dx - x * dy) / (y * y)
      PowC (x, dx) (n, _)  -> n * x ** (n-1) * dx
      NegC (_, dx)         -> - dx

      -- | Transcendental functions

      ExpF     (x, dx) -> dx * ExpF x
      LogF     (x, dx) -> dx / x
      SqrtF    (x, dx) -> dx / (2 * SqrtF x)
      
      CosF     (x, dx) -> - dx * SinF x
      SinF     (x, dx) -> dx * CosF x
      TanF     (x, dx) -> dx * (1 + (TanF x) ** 2)

      ArccosF  (x, dx) -> - dx * (SqrtF (1 - x ** 2))
      ArcsinF  (x, dx) -> dx * (SqrtF (1 - x ** 2))
      ArctanF  (x, dx) -> dx / (1 + x ** 2)
      
      CoshF    (x, dx) -> dx * SinhF x
      SinhF    (x, dx) -> dx * CoshF x
      TanhF    (x, dx) -> dx * (1 - (TanhF x) ** 2)

      ArccoshF (x, dx) -> - dx * (SqrtF (x ** 2 - 1))
      ArcsinhF (x, dx) -> dx * (SqrtF (1 + x ** 2))
      ArctanhF (x, dx) -> dx / (1 - x ** 2)

      ExpC     (x, dx) -> dx * ExpC x
      LogC     (x, dx) -> dx / x
      SqrtC    (x, dx) -> dx / (2 * SqrtC x)

      CosC     (x, dx) -> - dx * SinC x
      SinC     (x, dx) -> dx * CosC x
      TanC     (x, dx) -> dx * (1 + (TanC x) ** 2)

      CoshC    (x, dx) -> dx * SinhC x
      SinhC    (x, dx) -> dx * CoshC x
      TanhC    (x, dx) -> dx * (1 - (TanhC x) ** 2)

      -- | Type conversions

      I2R  (_, dx) -> I2R dx
      R2C  (_, dx) -> R2C dx
      C2R2 (_, dx) -> C2R2 dx

      _ -> error "not implemented for this expression"

      -- TO DO: Deal with LocalLet