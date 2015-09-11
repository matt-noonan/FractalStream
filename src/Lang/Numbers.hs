
module Lang.Numbers
    ( R(R)
    , C(C)
    , complex
    , i
    , realPart
    , imagPart
    , coords
    , conj
    , norm2
    ) where


data R = R {-# UNPACK #-}!Double deriving (Eq, Ord, Show)
data C = C {-# UNPACK #-}!Double {-# UNPACK #-}!Double deriving (Ord, Eq)

instance Show C where
    show (C x y) = show x ++ (if y < 0 then " - " else " + ") ++ show y ++ "i"

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
    exp z = complex (r * cos theta) (r * sin theta)
        where (r, theta) = coords z
    log _  = error "unimplemented"
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



