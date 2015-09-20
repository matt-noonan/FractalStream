{- |
Module      : Lang.Numbers
Description : Unboxed real and complex numbers.
-}
module Lang.Numbers
    ( R(R)
    , C(C)
    , R2(R2)
    , complex
    , i
    , realPart
    , imagPart
    , coords
    , conj
    , norm2
    ) where

data R  = R  {-# UNPACK #-}!Double deriving (Eq, Ord, Show)
data C  = C  {-# UNPACK #-}!Double {-# UNPACK #-}!Double deriving (Ord, Eq)
data R2 = R2 {-# UNPACK #-}!Double {-# UNPACK #-}!Double deriving (Eq,Ord,Show)

instance Show C where
    show (C x y) = show x ++ (if y < 0 then " - " else " + ") ++ show (abs y) ++ "i"

i :: C
i = C 0 1

complex :: Double -> Double -> C

realPart :: C -> Double
realPart = fst . coords

imagPart :: C -> Double
imagPart = snd . coords

coords :: C -> (Double, Double)
conj :: C -> C
norm2 :: C -> Double

complex x y = C x y
coords (C x y) = (x, y)
conj (C x y) = C x (-y)
norm2 z = realPart $ z * conj z

instance Num C where
    z + z' = complex (x + x') (y + y')
        where (x , y ) = coords z
              (x', y') = coords z'
    z * z' = complex (x * x' - y * y') (x * y' + y * x')
        where (x , y ) = coords z
              (x', y') = coords z'
    abs z = complex (sqrt $ norm2 z) 0
    signum z = if n == 0 then z else complex (x / n) (y / n)
        where (x,y) = coords z
              n = realPart $ abs z
    fromInteger n = complex (fromInteger n) 0
    negate z = complex (-x) (-y) where (x,y) = coords z

instance Fractional C where
    recip z = complex (x / n2) (-y / n2)
        where n2 = norm2 z
              (x,y) = coords z
    fromRational r = complex (fromRational r) 0

instance Floating C where
    pi   = complex pi 0
    exp z = complex (exp x * cos y) (exp x * sin y)
        where (x, y) = coords z
    log z  = complex ((log $ norm2 z) / 2) (atan2 y x)
        where (x,y) = coords z
    sin _  = error "unimplemented"
    cos _  = error "unimplemented"
    tan _  = error "unimplemented"
    asin _ = error "unimplemented"
    acos _ = error "unimplemented"
    atan _ = error "unimplemented"
    sinh _  = error "unimplemented"
    cosh _  = error "unimplemented"
    tanh _  = error "unimplemented"
    asinh _ = error "unimplemented"
    acosh _ = error "unimplemented"
    atanh _ = error "unimplemented"
    sqrt = error "unimplemented"

instance Num R where
    (R x) + (R y) = R $ x + y
    (R x) * (R y) = R $ x * y
    abs (R x) = R $ abs x
    signum (R x) = R $ signum x
    fromInteger = R . fromInteger
    negate (R x) = R $ negate x

instance Fractional R where
    recip (R x) = R $ recip x
    fromRational = R . fromRational

instance Floating R where
    pi = R pi
    sqrt (R x) = R $ sqrt x
    exp (R x) = R $ exp x
    log (R x) = R $ log x
    sin (R x) = R $ sin x
    cos (R x) = R $ cos x
    tan (R x) = R $ tan x
    acos (R x) = R $ acos x
    asin (R x) = R $ asin x
    atan (R x) = R $ atan x
    sinh (R x) = R $ sinh x
    cosh (R x) = R $ cosh x
    tanh (R x) = R $ tanh x
    acosh (R x) = R $ acosh x
    asinh (R x) = R $ asinh x
    atanh (R x) = R $ atanh x



