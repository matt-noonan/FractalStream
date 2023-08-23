module Language.Type
  ( FSType(..)
  , type HaskellType
  , Scalar(..)
  , TypeProxy(..)
  , SomeType(..)
  , sameHaskellType
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
import Data.Color (Color, colorToRGB)
import Data.Complex
import Data.List (intercalate)
import Data.Kind

data FSType
  = VoidT
  | BooleanT
  | IntegerT
  | RealT
  | ComplexT
  | RationalT
  | Pair FSType FSType
  | ColorT
  | ImageT
  | ListT FSType
  deriving (Eq, Ord, Show)

-- | Constant values for scalar types
type family HaskellType (t :: FSType) :: Type where
  HaskellType 'BooleanT   = Bool
  HaskellType 'IntegerT   = Int64
  HaskellType 'RealT      = Double
  HaskellType 'ComplexT   = Complex Double
  HaskellType 'RationalT  = (Int64, Int64)
  HaskellType 'ColorT     = Color
  HaskellType ('Pair x y) = (HaskellType x, HaskellType y)
  HaskellType 'VoidT      = ()
  HaskellType 'ImageT     = Int
  HaskellType ('ListT x)  = [HaskellType x]

-- | Constant values for scalar types. Match on the
-- first argument to make the type of the second argument
-- known.
data Scalar (t :: FSType) where
  Scalar :: forall t. TypeProxy t -> HaskellType t -> Scalar t

instance Eq (Scalar t) where
  Scalar t x == Scalar _ y = case t of
    BooleanType  -> x == y
    IntegerType  -> x == y
    RealType     -> x == y
    ComplexType  -> x == y
    RationalType -> x == y
    ColorType    -> x == y
    PairType t1 t2 ->
      let (x1, x2) = x
          (y1, y2) = y
      in (Scalar t1 x1, Scalar t2 x2) == (Scalar t1 y1, Scalar t2 y2)
    VoidType     -> x == y
    ImageType    -> x == y
    ListType it -> map (Scalar it) x == map (Scalar it) y

instance Ord (Scalar t) where
  compare (Scalar t x) (Scalar _ y) = case t of
    BooleanType  -> compare x y
    IntegerType  -> compare x y
    RealType     -> compare x y
    ComplexType  -> compare (realPart x, imagPart x) (realPart y, imagPart y)
    RationalType -> compare x y
    ColorType    -> compare x y
    PairType t1 t2 ->
      let (x1, x2) = x
          (y1, y2) = y
      in compare (Scalar t1 x1, Scalar t2 x2) (Scalar t1 y1, Scalar t2 y2)
    VoidType     -> compare x y
    ImageType    -> compare x y
    ListType it -> compare (map (Scalar it) x) (map (Scalar it) y)

sameHaskellType :: TypeProxy t1 -> TypeProxy t2 -> Maybe (t1 :~: t2)
sameHaskellType v1 v2 = case v1 of
  BooleanType  -> case v2 of { BooleanType  -> Just Refl; _ -> Nothing }
  IntegerType  -> case v2 of { IntegerType  -> Just Refl; _ -> Nothing }
  RealType     -> case v2 of { RealType     -> Just Refl; _ -> Nothing }
  ComplexType  -> case v2 of { ComplexType  -> Just Refl; _ -> Nothing }
  RationalType -> case v2 of { RationalType -> Just Refl; _ -> Nothing }
  ColorType    -> case v2 of { ColorType    -> Just Refl; _ -> Nothing }
  VoidType     -> case v2 of { VoidType     -> Just Refl; _ -> Nothing }
  ImageType    -> case v2 of { ImageType    -> Just Refl; _ -> Nothing }
  PairType x y -> case v2 of
    PairType x' y' -> case (,) <$> sameHaskellType x x' <*> sameHaskellType y y' of
      Just (Refl, Refl) -> Just Refl
      Nothing           -> Nothing
    _ -> Nothing
  ListType x -> case v2 of
    ListType x' -> case sameHaskellType x x' of
      Just Refl -> Just Refl
      Nothing   -> Nothing
    _ -> Nothing

-- | Singleton values reflecting the type-level parameter @t@.
data TypeProxy (t :: FSType) where
  BooleanType  :: TypeProxy 'BooleanT
  IntegerType  :: TypeProxy 'IntegerT
  RealType     :: TypeProxy 'RealT
  ComplexType  :: TypeProxy 'ComplexT
  RationalType :: TypeProxy 'RationalT
  ColorType    :: TypeProxy 'ColorT
  PairType     :: (KnownType x, KnownType y) => TypeProxy x -> TypeProxy y -> TypeProxy ('Pair x y)
  VoidType     :: TypeProxy 'VoidT
  ImageType    :: TypeProxy 'ImageT
  ListType     :: KnownType x => TypeProxy x -> TypeProxy ('ListT x)

data SomeType where
  SomeType :: forall t. TypeProxy t -> SomeType

instance Show SomeType where
  show (SomeType t) = showType t

instance Eq SomeType where
  SomeType t1 == SomeType t2 = maybe False (const True) (sameHaskellType t1 t2)

class KnownType (t :: FSType)   where typeProxy :: TypeProxy t
instance KnownType 'BooleanT  where typeProxy = BooleanType
instance KnownType 'IntegerT  where typeProxy = IntegerType
instance KnownType 'RealT     where typeProxy = RealType
instance KnownType 'ComplexT  where typeProxy = ComplexType
instance KnownType 'RationalT where typeProxy = RationalType
instance KnownType 'ColorT    where typeProxy = ColorType
instance KnownType 'VoidT     where typeProxy = VoidType
instance KnownType 'ImageT    where typeProxy = ImageType
instance KnownType x => KnownType ('ListT x) where
  typeProxy = ListType (typeProxy @x)
instance (KnownType x, KnownType y) => KnownType ('Pair x y) where
  typeProxy = PairType (typeProxy @x) (typeProxy @y)

withKnownType :: TypeProxy t -> (KnownType t => a) -> a
withKnownType ty k = case ty of
  BooleanType  -> k
  IntegerType  -> k
  RealType     -> k
  ComplexType  -> k
  RationalType -> k
  ColorType    -> k
  VoidType     -> k
  ImageType    -> k
  PairType {}  -> k
  ListType {}  -> k

pattern Boolean_ :: forall (t :: FSType). () => (t ~ 'BooleanT) => HaskellType t -> Scalar t
pattern Boolean_ x = Scalar BooleanType x

pattern Integer_ :: forall (t :: FSType). () => (t ~ 'IntegerT) => HaskellType t -> Scalar t
pattern Integer_ x = Scalar IntegerType x

pattern Real_ :: forall (t :: FSType). () => (t ~ 'RealT) => HaskellType t -> Scalar t
pattern Real_ x    = Scalar RealType x

pattern Complex_ :: forall (t :: FSType). () => (t ~ 'ComplexT) => HaskellType t -> Scalar t
pattern Complex_  pair = Scalar ComplexType pair

pattern Rational_ :: forall (t :: FSType). () => (t ~ 'RationalT) => HaskellType t -> Scalar t
pattern Rational_ pair = Scalar RationalType pair

pattern Color_ :: forall (t :: FSType). () => (t ~ 'ColorT) => HaskellType t -> Scalar t
pattern Color_ c = Scalar ColorType c


showType :: TypeProxy t -> String
showType = \case
  BooleanType  -> "Boolean"
  IntegerType  -> "Integer"
  RealType     -> "Real"
  ComplexType  -> "Complex"
  RationalType -> "Rational"
  ColorType    -> "Color"
  VoidType     -> "Unit"
  ImageType    -> "Image"
  PairType x y -> "(" <> showType x <> " x " <> showType y <> ")"
  ListType x   -> "List " <> showType x

showValue :: TypeProxy t -> HaskellType t -> String
showValue ty v = case ty of
  BooleanType  -> if v then "true" else "false"
  IntegerType  -> show v
  RealType     -> show v
  ComplexType  -> let x :+ y = v
                   in show x <> " + " <> show y <> "i"
  RationalType -> let (x, y) = v
                   in show x <> " / " <> show y
  ColorType    -> show (colorToRGB v)
  VoidType     -> "n/a"
  ImageType    -> "(image)"
  PairType xt yt -> let (x, y) = v
                     in showValue xt x <> " , " <> showValue yt y
  ListType xt -> "[" ++ intercalate ", " (map (showValue xt) v) ++ "]"
