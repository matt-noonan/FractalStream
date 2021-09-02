{-# language AllowAmbiguousTypes #-}

module Language.Value.Evaluator
  ( evaluate
  , evaluator
  , ScalarType_
  , ScalarTypeOfBinding
  , partialEvaluate
  , constantFold
  ) where

import Language.Value
import Data.Indexed.Functor
import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits
import Fcf (Exp, Eval, Pure1)
import Unsafe.Coerce

-- | First-class family corresponding to ScalarType, suitable to use in a Context
data ScalarTypeOfBinding :: Symbol -> Type -> Exp *
type instance Eval (ScalarTypeOfBinding name t) = ScalarType t

-- | First-class family corresponding to ScalarType, suitable to use in a Context
data ScalarType_ :: Type -> Exp *
type instance Eval (ScalarType_ t) = ScalarType t

type family WithoutBinding (env :: Environment) (name :: Symbol) :: Environment where
  WithoutBinding ( '(name,t) ': env ) name = env
  WithoutBinding ( '(x,t) ': env )    name = '(x,t) ': WithoutBinding env name

-- | Perform partial evaluation, replacing uses of a specific
-- variable with the given value.
--
-- NOTE: This actually computes a Value env t -> Value env t transformation,
-- but hits the result with unsafeCoerce to transform env to
-- (env `WithoutBinding name). This is safe because the environment only
-- appears in the `Required name env ~ t` constraint on the Var
-- constructor, and this constraint doesn't carry any nontrivial
-- information. Since all references to 'name' are removed by this
-- transformation, the result type really *is* correct.
partialEvaluate :: forall env t name ty
                 . (Required name env ~ ty, KnownSymbol name)
                => Proxy name
                -> ScalarProxy ty
                -> ScalarType ty
                -> Value env t
                -> Value (env `WithoutBinding` name) t
partialEvaluate name ty v =
  unsafeCoerce . (indexedFold @(Pure1 (Value env)) @(Value env) @(ValueF env) $ \case

    Var name' ty' -> case sameSymbol name name' of
      Just Refl -> Fix (Const (Scalar ty v))
      Nothing   -> Fix (Var name' ty')
    etc -> Fix etc
  )

-- | ValueOrConstant is used during constant folding. It
-- can represent either a value of type @Value env t@,
-- or else a constant of type @ScalarType t@.
data ValueOrConstant (env :: Environment) (t :: Type) where
  V :: Value  env t -> ValueOrConstant env t
  C :: Scalar t -> ValueOrConstant env t

-- | Turn a ValueOrConstant into a Value. This can
-- always be done.
toV :: ValueOrConstant env t -> Value env t
toV = \case
  V v -> v
  C c -> Fix (Const c)

-- | Turn a ValueOrConstant into a constant. This
-- could fail, hence the Maybe.
toC :: ValueOrConstant env t -> Maybe (ScalarType t)
toC = \case { V _ -> Nothing; C (Scalar _ c) -> Just c }

-- | Perform a constant-folding transformation over a 'Value'
constantFold :: forall env t. Value env t -> Value env t
constantFold =
  toV . (indexedFold @(Pure1 (ValueOrConstant env)) @(Value env) @(ValueF env) $ \case
    -- Base cases: constants are constant, variables aren't.
    Const c     -> C c
    Var name ty -> V (Fix (Var name ty))

    -- For all other constructors: if all children are constants,
    -- use the 'evaluator' algebra to compute the value of the constructor.
    -- If not all children are constants, create a non-constant Value.
    etc -> case itraverse (const toC) etc of
      Nothing -> V (Fix (imap (const toV) etc))
      Just  c -> C (Scalar (toIndex etc) (evaluator impossible c))
  )
 where
   impossible _ _ = error "unreachable, Var constructor is already handled in constantFold"

-- | Evaluate the (normal) value corresponding to a 'Value', given values
-- for each variable that appears.
evaluate :: forall env t. Context ScalarTypeOfBinding env -> Value env t -> ScalarType t
evaluate context =
  indexedFold @ScalarType_ @(Value env) @(ValueF env) (evaluator (getBinding context))

-- | Evaluation algebra
evaluator :: forall env t
           . (forall name ty. (KnownSymbol name, Required name env ~ ty)
              => Proxy name -> ScalarProxy ty -> ScalarType ty)
          -> ValueF env ScalarType_ t
          -> ScalarType t
evaluator lookUp = \case

    Const (Scalar _ v) -> v
    Var name ty -> lookUp name ty

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
    NegF x   -> negate x

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
    NegI x   -> negate x

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
