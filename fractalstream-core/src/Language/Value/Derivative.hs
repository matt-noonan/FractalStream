{-# language AllowAmbiguousTypes, UndecidableInstances #-}

module Language.Value.Derivative
  (derivative) where

import FractalStream.Prelude

import Data.Indexed.Functor
import Language.Parser.SourceRange
import Language.Typecheck
import Language.Value

-- Computes the partial derivative of a Value et with respect to z
derivative :: forall et. String -> SourceRange -> Value et -> TC (Value et)
derivative z sr = indexedFoldWithOriginalM derivativeRules
  where
    derivativeRules :: forall s. ValueF (FIX ValueF :*: FIX ValueF) s -> TC (Value s)
    derivativeRules = \case

      -- | Constant case
      Const (Scalar ty _) -> case ty of
        ComplexType -> pure 0
        RealType    -> pure 0
        IntegerType -> pure 0
        _           -> throwError $ DiffNotImplemented sr z

      -- | Other variables are assumed to be constant with respect to z 
      Var name ty _ -> case ty of
        ComplexType -> if symbolVal name == z
                       then pure $ Const $ Scalar ComplexType 1
                       else pure $ Const $ Scalar ComplexType 0
        RealType    -> if symbolVal name == z
                       then pure $ Const $ Scalar RealType 1
                       else pure $ Const $ Scalar RealType 0
        _           -> throwError $ DiffNotImplemented sr z

      -- | Basic algebra

      AddF (_, dx) (_, dy) -> pure $ dx + dy
      SubF (_, dx) (_, dy) -> pure $ dx - dy
      MulF (x, dx) (y, dy) -> pure $ y * dx + x * dy
      DivF (x, dx) (y, dy) -> pure $ (y * dx - x * dy) / (y * y)
      PowF (x, dx) (n, _)  -> pure $ n * x ** (n-1) * dx
      NegF (_, dx)         -> pure $ negate dx

      AddC (_, dx) (_, dy) -> pure $ dx + dy
      SubC (_, dx) (_, dy) -> pure $ dx - dy
      MulC (x, dx) (y, dy) -> pure $ y * dx + x * dy
      DivC (x, dx) (y, dy) -> pure $ (y * dx - x * dy) / (y * y)
      PowC (x, dx) (n, _)  -> pure $ n * x ** (n-1) * dx
      NegC (_, dx)         -> pure $ negate dx

      -- | Transcendental functions

      ExpF     (x, dx) -> pure $ dx * ExpF x
      LogF     (x, dx) -> pure $ dx / x
      SqrtF    (x, dx) -> pure $ dx / (2 * SqrtF x)
      
      CosF     (x, dx) -> pure $ negate dx * SinF x
      SinF     (x, dx) -> pure $ dx * CosF x
      TanF     (x, dx) -> pure $ dx * (1 + TanF x ** 2)

      ArccosF  (x, dx) -> pure $ negate dx * SqrtF (1 - x ** 2)
      ArcsinF  (x, dx) -> pure $ dx * SqrtF (1 - x ** 2)
      ArctanF  (x, dx) -> pure $ dx / (1 + x ** 2)
      
      CoshF    (x, dx) -> pure $ dx * SinhF x
      SinhF    (x, dx) -> pure $ dx * CoshF x
      TanhF    (x, dx) -> pure $ dx * (1 - TanhF x ** 2)

      ArccoshF (x, dx) -> pure $ negate dx * SqrtF (x ** 2 - 1)
      ArcsinhF (x, dx) -> pure $ dx * SqrtF (1 + x ** 2)
      ArctanhF (x, dx) -> pure $ dx / (1 - x ** 2)
      
      ExpC     (x, dx) -> pure $ dx * ExpC x
      LogC     (x, dx) -> pure $ dx / x
      SqrtC    (x, dx) -> pure $ dx / (2 * SqrtC x)

      CosC     (x, dx) -> pure $ negate dx * SinC x
      SinC     (x, dx) -> pure $ dx * CosC x
      TanC     (x, dx) -> pure $ dx * (1 + TanC x ** 2)

      CoshC    (x, dx) -> pure $ dx * SinhC x
      SinhC    (x, dx) -> pure $ dx * CoshC x
      TanhC    (x, dx) -> pure $ dx * (1 - TanhC x ** 2)

      -- | Type conversions

      I2R  (_, dx) -> pure $ I2R dx
      R2C  (_, dx) -> pure $ R2C dx
      C2R2 (_, dx) -> pure $ C2R2 dx

      _ -> throwError $ DiffNotImplemented sr z