module Language.Untyped.Evaluator
  ( evaluate
  , SomeScalar(..)
  ) where

import Language.Untyped.Value
import Language.Type

import Data.Map (Map)
import qualified Data.Map as Map

data SomeScalar where
  S :: forall t. ScalarProxy t -> ScalarType t -> SomeScalar

data SomeScalarPair where
  S2 :: forall t
      . Num (ScalarType t)
     => ScalarProxy t
     -> ScalarType t
     -> ScalarType t
     -> SomeScalar

evaluate :: Map String SomeType
         -> Value
         -> Maybe SomeType
evaluate ctx = foldValueM $ \case
  ConstB b -> ok BooleanProxy b
  ConstI n -> ok IntegerProxy n
  ConstF x -> ok RealProxy x
  ConstC x y -> ok ComplexProxy (x :+ y)
  ConstColor c -> ok ColorProxy c
  Var s -> Map.lookup s ctx
  PairV (S t1 x) (S t2 y) -> ok (PairProxy t1 t2) (x, y)
  ProjV1 (S (PairProxy t1 _) (x, _)) = S t1 x
  ProjV2 (S (PairProxy _ t2) (_, y)) = S t2 y
  Add x y = joinTy (+) x y
  Sub x y = joinTy (-) x y
  Mul x y = joinTy (*) x y
  Div x y = joinTy (div) x y

  _ -> Nothing

 where
  ok :: ScalarProxy ty -> ScalarType ty -> Maybe SomeScalar
  ok t v = Just (S t v)

  joinTy :: (forall t. Num (ScalarType t) => ScalarType t -> ScalarType t -> ScalarType t)
         -> SomeType
         -> SomeType
         -> Maybe SomeType
  joinTy op (S ComplexProxy z) (S ComplexProxy w) = S ComplexProxy (z `op` w)
  joinTy op (S RealProxy x) (S ComplexProxy w) = S ComplexProxy ((x :+ 0) `op` w)
  joinTy op (S ComplexProxy z) (S RealProxy x) = S ComplexProxy (z `op` (x :+ 0))
  joinTy op (S ComplexProxy z) (S IntegerProxy n) = S ComplexProxy (z `op` fromIntegral n)
  joinTy op (S IntegerProxy n) (S ComplexProxy w) = S ComplexProxy (fromIntegral n `op` w)
