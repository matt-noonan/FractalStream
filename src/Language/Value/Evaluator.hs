module Language.Value.Evaluator
  ( evaluate
  ) where

import Fcf
import Language.Value
import Data.Indexed.Functor

data ScalarType_ :: Type -> Exp *
type instance Eval (ScalarType_ t) = ScalarType t

evaluate :: forall env t. Context ScalarType_ env -> Value env t -> ScalarType t
evaluate context =
  indexedFold @ScalarType_ @(Value env) @(ValueF env) $ \case

    Const (Scalar _ v) -> v
    Var name ty -> getBinding context name ty

    PairV _ x y     -> (x, y)
    ProjV1 _ (x, _) -> x
    ProjV2 _ (_, y) -> y

    AddF x y -> x + y
    SubF x y -> x - y
    MulF x y -> x * y
    DivF x y -> x / y
    ModF _ _ -> error "TODO"
    PowF x n -> x ^^ n
    AbsF x   -> abs x

    ExpF x -> exp x
    LogF x -> log x

    SinF x -> sin x
    CosF x -> cos x
    TanF x -> tan x
    ArcsinF x -> asin x
    ArccosF x -> acos x
    ArctanF x -> atan x
    Arctan2F x y -> atan2 x y

    AddI x y -> x + y
    SubI x y -> x - y
    MulI x y -> x * y
    DivI x y -> x `div` y
    ModI x y -> x `mod` y
    PowI x n -> x ^ n
    AbsI x   -> abs x

    Or  x y -> x || y
    And x y -> x && y
    Not x   -> not x

    ITE _ b yes no -> if b then yes else no

    Eql t x y -> Scalar t x == Scalar t y
    NEq t x y -> Scalar t x /= Scalar t y

    LEI x y -> x <= y
    LEF x y -> x <= y
    GEI x y -> x >= y
    GEF x y -> x >= y
    LTI x y -> x < y
    LTF x y -> x < y
    GTI x y -> x > y
    GTF x y -> x > y
