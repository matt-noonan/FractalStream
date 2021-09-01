{-# language UndecidableInstances #-}

module Language.Type
  ( Type(..)
  , type ScalarType
  , Scalar(..)
  , ScalarProxy(..)
  , sameScalarType
  , type Environment
  , Int64
  , Symbol
  , type Lookup
  , type Required
  , type NotPresent
  , pattern Boolean_
  , pattern Integer_
  , pattern Real_
  , pattern Complex_
  , pattern Rational_
  , pattern Color_
  , showType
  , showValue
  , Context(..)
  , getBinding
  ) where

import Data.Int
import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Type.Equality ((:~:)(..))
import Data.Proxy
import Fcf (Exp, Eval)
import Color.Color (Color, colorToRGB)

data Type
  = VoidT
  | BooleanT
  | IntegerT
  | RealT
  | ComplexT
  | RationalT
  | Pair Type Type
  | ColorT

-- | Constant values for scalar types
type family ScalarType (t :: Type) :: * where
  ScalarType 'BooleanT   = Bool
  ScalarType 'IntegerT   = Int64
  ScalarType 'RealT      = Double
  ScalarType 'ComplexT   = (Double, Double)
  ScalarType 'RationalT  = (Int64, Int64)
  ScalarType 'ColorT     = Color
  ScalarType ('Pair x y) = (ScalarType x, ScalarType y)
  ScalarType 'VoidT      = ()

-- | Constant values for scalar types. Match on the
-- first argument to make the type of the second argument
-- known.
data Scalar (t :: Type) where
  Scalar :: forall t. ScalarProxy t -> ScalarType t -> Scalar t

instance Eq (Scalar t) where
  Scalar t x == Scalar _ y = case t of
    BooleanProxy  -> x == y
    IntegerProxy  -> x == y
    RealProxy     -> x == y
    ComplexProxy  -> x == y
    RationalProxy -> x == y
    ColorProxy    -> x == y
    PairProxy t1 t2 ->
      let (x1, x2) = x
          (y1, y2) = y
      in (Scalar t1 x1, Scalar t2 x2) == (Scalar t1 y1, Scalar t2 y2)
    VoidProxy     -> x == y

instance Ord (Scalar t) where
  compare (Scalar t x) (Scalar _ y) = case t of
    BooleanProxy  -> compare x y
    IntegerProxy  -> compare x y
    RealProxy     -> compare x y
    ComplexProxy  -> compare x y
    RationalProxy -> compare x y
    ColorProxy    -> compare x y
    PairProxy t1 t2 ->
      let (x1, x2) = x
          (y1, y2) = y
      in compare (Scalar t1 x1, Scalar t2 x2) (Scalar t1 y1, Scalar t2 y2)
    VoidProxy     -> compare x y

sameScalarType :: ScalarProxy t1 -> ScalarProxy t2 -> Maybe (t1 :~: t2)
sameScalarType v1 v2 = case v1 of
  BooleanProxy  -> case v2 of { BooleanProxy  -> Just Refl; _ -> Nothing }
  IntegerProxy  -> case v2 of { IntegerProxy  -> Just Refl; _ -> Nothing }
  RealProxy     -> case v2 of { RealProxy     -> Just Refl; _ -> Nothing }
  ComplexProxy  -> case v2 of { ComplexProxy  -> Just Refl; _ -> Nothing }
  RationalProxy -> case v2 of { RationalProxy -> Just Refl; _ -> Nothing }
  ColorProxy    -> case v2 of { ColorProxy    -> Just Refl; _ -> Nothing }
  VoidProxy     -> case v2 of { VoidProxy     -> Just Refl; _ -> Nothing }
  PairProxy x y -> case v2 of
    PairProxy x' y' -> case (,) <$> sameScalarType x x' <*> sameScalarType y y' of
      Just (Refl, Refl) -> Just Refl
      Nothing           -> Nothing
    _ -> Nothing

-- | Singleton values reflecting the type-level parameter @t@.
data ScalarProxy (t :: Type) where
  BooleanProxy  :: ScalarProxy 'BooleanT
  IntegerProxy  :: ScalarProxy 'IntegerT
  RealProxy     :: ScalarProxy 'RealT
  ComplexProxy  :: ScalarProxy 'ComplexT
  RationalProxy :: ScalarProxy 'RationalT
  ColorProxy    :: ScalarProxy 'ColorT
  PairProxy     :: ScalarProxy x -> ScalarProxy y -> ScalarProxy ('Pair x y)
  VoidProxy     :: ScalarProxy 'VoidT

pattern Boolean_ :: forall (t :: Type). () => (t ~ 'BooleanT) => ScalarType t -> Scalar t
pattern Boolean_ x = Scalar BooleanProxy x

pattern Integer_ :: forall (t :: Type). () => (t ~ 'IntegerT) => ScalarType t -> Scalar t
pattern Integer_ x = Scalar IntegerProxy x

pattern Real_ :: forall (t :: Type). () => (t ~ 'RealT) => ScalarType t -> Scalar t
pattern Real_ x    = Scalar RealProxy x

pattern Complex_ :: forall (t :: Type). () => (t ~ 'ComplexT) => ScalarType t -> Scalar t
pattern Complex_  pair = Scalar ComplexProxy pair

pattern Rational_ :: forall (t :: Type). () => (t ~ 'RationalT) => ScalarType t -> Scalar t
pattern Rational_ pair = Scalar RationalProxy pair

pattern Color_ :: forall (t :: Type). () => (t ~ 'ColorT) => ScalarType t -> Scalar t
pattern Color_ c = Scalar ColorProxy c


showType :: ScalarProxy t -> String
showType = \case
  BooleanProxy  -> "Boolean"
  IntegerProxy  -> "Integer"
  RealProxy     -> "Real"
  ComplexProxy  -> "Complex"
  RationalProxy -> "Rational"
  ColorProxy    -> "Color"
  VoidProxy     -> "Unit"
  PairProxy x y -> showType x <> " x " <> showType y

showValue :: ScalarProxy t -> ScalarType t -> String
showValue ty v = case ty of
  BooleanProxy  -> if v then "true" else "false"
  IntegerProxy  -> show v
  RealProxy     -> show v
  ComplexProxy  -> let (x, y) = v
                   in show x <> " + " <> show y <> "i"
  RationalProxy -> let (x, y) = v
                   in show x <> " / " <> show y
  ColorProxy    -> show (colorToRGB v)
  VoidProxy     -> "n/a"
  PairProxy xt yt -> let (x, y) = v
                     in showValue xt x <> " , " <> showValue yt y

-- | An 'Environment' is a map from symbols to 'Type's.
type Environment = [(Symbol, Type)]

-- | Look up the 'Type' of a name in the environment.
type family Lookup
    (name :: Symbol) (env :: [(Symbol, Type)]) :: Maybe Type where
  Lookup _ '[] = 'Nothing
  Lookup name ('(name, t) ': _) = 'Just t
  Lookup name (_ ': env) = Lookup name env

-- | Evaluate to the 'Type' of @name@ in the environment @env@,
-- or raise a type error if the name is not present.
type Required name env
  = Required_impl name (Lookup name env)

type family Required_impl
    (name :: Symbol) (result :: Maybe Type) :: Type where
  Required_impl name 'Nothing =
    TypeError ('Text "No variable named " ':<>:
               'Text name ':<>: 'Text " is in scope")
  Required_impl name ('Just t) = t

-- | Raise a type error if the given name is already
-- present in the environment.
type NotPresent name env
  = NotPresent_impl name (Lookup name env)

type family NotPresent_impl
    (name :: Symbol) (result :: Maybe Type) :: Constraint where
  NotPresent_impl name ('Just _) =
    TypeError ('Text "The name " ':<>: 'Text name ':<>:
               'Text " shadows another variable in scope")
  NotPresent_impl name 'Nothing = ()

data Context (value :: Type -> Exp *) (env :: Environment) where
  EmptyContext :: forall value. Context value '[]
  Bind :: forall name ty env value
        . (NotPresent name env, KnownSymbol name)
       => Proxy name
       -> ScalarProxy ty
       -> Eval (value ty)
       -> Context value env
       -> Context value ( '(name, ty) ': env)

getBinding :: forall name t env value
            . (Required name env ~ t, KnownSymbol name)
           => Context value env -> Proxy name -> ScalarProxy t -> Eval (value t)
getBinding ctx0 name ty = go ctx0
  where
    go :: forall env'. Context value env' -> Eval (value t)
    go = \case
      EmptyContext -> error "unreachable due to (Required name env ~ t) constraint"
      Bind name' ty' v ctx ->
        case sameSymbol name name' of
          Nothing -> go ctx
          Just _  -> case sameScalarType ty ty' of
            Just Refl -> v
            Nothing   -> error "unreachable due to (NotPresent name env) constraint in Bind constructor"
