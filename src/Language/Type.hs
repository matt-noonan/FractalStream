{-# language UndecidableInstances #-}

module Language.Type
  ( Type(..)
  , type ScalarType
  , Scalar(..)
  , ScalarProxy(..)
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
  ) where

import Data.Int
import GHC.TypeLits
import GHC.Exts (Constraint)
import Color.Color (Color)

data Type
  = VoidT
  | BooleanT
  | IntegerT
  | RealT
  | ComplexT
  | RationalT
  | Pair Type Type
  | Function Type Type
  | ColorT
  | ViewportT
  | SettingsT

-- | Constant values for scalar types
type family ScalarType (t :: Type) :: * where
  ScalarType 'BooleanT  = Bool
  ScalarType 'IntegerT  = Int64
  ScalarType 'RealT     = Double
  ScalarType 'ComplexT  = (Double, Double)
  ScalarType 'RationalT = (Int64, Int64)
  ScalarType 'ColorT    = Color

-- | Constant values for scalar types. Match on the
-- first argument to make the type of the second argument
-- known.
data Scalar (t :: Type) where
  Scalar :: forall t. ScalarProxy t -> ScalarType t -> Scalar t

-- | Singleton values reflecting the type-level parameter @t@.
data ScalarProxy (t :: Type) where
  BooleanProxy  :: ScalarProxy 'BooleanT
  IntegerProxy  :: ScalarProxy 'IntegerT
  RealProxy     :: ScalarProxy 'RealT
  ComplexProxy  :: ScalarProxy 'ComplexT
  RationalProxy :: ScalarProxy 'RationalT
  ColorProxy    :: ScalarProxy 'ColorT

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
