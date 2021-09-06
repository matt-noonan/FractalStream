module Language.Type
  ( Type(..)
  , type ScalarType
  , Scalar(..)
  , ScalarProxy(..)
  , sameScalarType
  , Int64
  , Symbol
  , Complex(..)
  , pattern Boolean_
  , pattern Integer_
  , pattern Real_
  , pattern Complex_
  , pattern Rational_
  , pattern Color_
  , showType
  , showValue
  , KnownType(..)
  , withKnownType
  ) where

import Data.Int
import GHC.TypeLits
import Data.Type.Equality ((:~:)(..))
import Color.Color (Color, colorToRGB)
import Data.Complex

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
  ScalarType 'ComplexT   = Complex Double
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
    ComplexProxy  -> compare (realPart x, imagPart x) (realPart y, imagPart y)
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
  PairProxy     :: (KnownType x, KnownType y) => ScalarProxy x -> ScalarProxy y -> ScalarProxy ('Pair x y)
  VoidProxy     :: ScalarProxy 'VoidT

class KnownType (t :: Type)  where typeProxy :: ScalarProxy t
instance KnownType 'BooleanT where typeProxy = BooleanProxy
instance KnownType 'IntegerT where typeProxy = IntegerProxy
instance KnownType 'RealT    where typeProxy = RealProxy
instance KnownType 'ComplexT where typeProxy = ComplexProxy
instance KnownType 'RationalT where typeProxy = RationalProxy
instance KnownType 'ColorT    where typeProxy = ColorProxy
instance KnownType 'VoidT     where typeProxy = VoidProxy
instance (KnownType x, KnownType y) => KnownType ('Pair x y) where
  typeProxy = PairProxy (typeProxy @x) (typeProxy @y)

withKnownType :: ScalarProxy t -> (KnownType t => a) -> a
withKnownType ty k = case ty of
  BooleanProxy  -> k
  IntegerProxy  -> k
  RealProxy     -> k
  ComplexProxy  -> k
  RationalProxy -> k
  ColorProxy    -> k
  VoidProxy     -> k
  PairProxy {}  -> k

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
  ComplexProxy  -> let x :+ y = v
                   in show x <> " + " <> show y <> "i"
  RationalProxy -> let (x, y) = v
                   in show x <> " / " <> show y
  ColorProxy    -> show (colorToRGB v)
  VoidProxy     -> "n/a"
  PairProxy xt yt -> let (x, y) = v
                     in showValue xt x <> " , " <> showValue yt y
