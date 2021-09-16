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
import Data.Color

import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits
import Fcf (Exp, Eval, Pure1)
import Unsafe.Coerce
import Numeric.Extras

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
-- appears in the `NameIsPresent` proof in the Var
-- constructor, and this proof doesn't carry any nontrivial
-- information. Since all references to 'name' are removed by this
-- transformation, the result type really *is* correct.
partialEvaluate :: forall env t name ty
                 . (KnownSymbol name)
                => Proxy name
                -> ScalarProxy ty
                -> ScalarType ty
                -> NameIsPresent name ty env
                -> Value env t
                -> Value (env `WithoutBinding` name) t
partialEvaluate name ty v pf =
  unsafeCoerce . (indexedFold @(Pure1 (Value env)) @(Value env) @(ValueF env) $ \case

    Var name' ty' pf' -> case sameSymbol name name' of
      Just Refl -> case typeIsUnique pf pf' of
        Refl -> Fix (Const (Scalar ty v))
      Nothing   -> Fix (Var name' ty' pf')
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
    Var name ty pf -> V (Fix (Var name ty pf))

    -- For all other constructors: if all children are constants,
    -- use the 'evaluator' algebra to compute the value of the constructor.
    -- If not all children are constants, create a non-constant Value.
    etc -> case itraverse (const toC) etc of
      Nothing -> V (Fix (imap (const toV) etc))
      Just  c -> C (Scalar (toIndex etc) (evaluator impossible c))
  )
 where
   impossible = error "unreachable, Var constructor is already handled in constantFold"

-- | Evaluate the (normal) value corresponding to a 'Value', given values
-- for each variable that appears.
evaluate :: forall env t. Context ScalarTypeOfBinding env -> Value env t -> ScalarType t
evaluate context =
  indexedFold @ScalarType_ @(Value env) @(ValueF env) (evaluator (getBinding context))

-- | Evaluation algebra
evaluator :: forall env t
           . (forall name ty. (KnownSymbol name, KnownType ty)
              => NameIsPresent name ty env -> ScalarType ty)
          -> ValueF env ScalarType_ t
          -> ScalarType t
evaluator lookUp = \case

    Const (Scalar _ v) -> v
    Var _name ty pf -> withKnownType ty (lookUp pf)

    PairV _ x y     -> (x, y)
    ProjV1 _ (x, _) -> x
    ProjV2 _ (_, y) -> y

    AddF x y -> x + y
    SubF x y -> x - y
    MulF x y -> x * y
    DivF x y -> x / y
    ModF x y -> fmod x y
    PowF x n -> x ** n
    AbsF x   -> abs x
    NegF x   -> negate x

    ExpF x -> exp x
    LogF x -> log x
    SqrtF x -> sqrt x

    SinF x -> sin x
    CosF x -> cos x
    TanF x -> tan x
    ArcsinF x -> asin x
    ArccosF x -> acos x
    ArctanF x -> atan x
    Arctan2F x y -> atan2 x y

    SinhF x -> sinh x
    CoshF x -> cosh x
    TanhF x -> tanh x
    ArcsinhF x -> asinh x
    ArccoshF x -> acosh x
    ArctanhF x -> atanh x

    AddC x y -> x + y
    SubC x y -> x - y
    MulC x y -> x * y
    DivC x y -> x / y
    PowC x n -> x ** n
    NegC x   -> negate x

    ExpC x -> exp x
    LogC x -> log x
    SqrtC x -> sqrt x

    SinC x -> sin x
    CosC x -> cos x
    TanC x -> tan x

    SinhC x -> sinh x
    CoshC x -> cosh x
    TanhC x -> tanh x

    AbsC (x :+ y) -> sqrt (x ** 2 + y ** 2)
    ArgC (x :+ y) -> atan2 y x
    ReC (x :+ _) -> x
    ImC (_ :+ y) -> y
    ConjC (x :+ y) -> x :+ negate y

    I2R n -> fromIntegral n
    R2C x -> x :+ 0

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

    RGB r g b     -> rgbToColor ( round (255 * r)
                                , round (255 * g)
                                , round (255 * b) )
    Blend s c1 c2 -> mixColors s c2 c1

    InvertRGB c   -> invertColor c

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
