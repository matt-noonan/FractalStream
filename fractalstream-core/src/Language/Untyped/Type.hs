module Language.Untyped.Type
  (
  ) where

data Ty
  = BoolTy
  | IntegerTy
  | RealTy
  | ComplexTy
  | ColorTy
  | PairTy Ty Ty
  | TyVar !Int
  deriving (Eq, Ord, Show)
