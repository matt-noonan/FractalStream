{- |
Module      : Lang.Planar
Description : The Planar typeclass, for types with maps to and from R^2.
-}
module Lang.Planar ( Planar (..)
                   , R
                   , Rectangle()
                   , Viewport (Viewport)
                   , rectangle
                   , upperLeft
                   , lowerRight
                   , rectPoints
                   , translateRect
                   , convertRect
                   , intersectsRect
                   , dimensions
                   ) where

import Lang.Numbers

-- | The class of types which can be converted to and from $\mathbb{R}^2$.
--   Instances should satisfy the law (fromCoords . toCoords == id).
class Planar a where
    toCoords :: a -> (Double,Double)
    fromCoords :: (Double,Double) -> a

-- | A representation of a generic rectangle.
data Rectangle a = Rectangle { _upperLeft :: a, _lowerRight :: a } deriving (Eq, Ord, Show)

-- | Extract the upper-left point of a rectangle.
upperLeft :: Rectangle a -> a
upperLeft = _upperLeft

-- | Extract the lower-right point of a rectangle.
lowerRight :: Rectangle a -> a
lowerRight = _lowerRight

-- | Build a rectangle.
rectangle :: Planar a
          => a  -- ^ The upper-left corner.
          -> a  -- ^ The lower-right corner.
          -> Rectangle a
rectangle p p' = Rectangle { _upperLeft  = fromCoords $ (min x x', min y y')
                           , _lowerRight = fromCoords $ (max x x', max y y')
                           }
    where (x , y ) = toCoords p
          (x', y') = toCoords p'

-- | Move a rectangle by a given displacement vector.
translateRect :: Planar a => (Double, Double) -> Rectangle a -> Rectangle a
translateRect (dx,dy) rect = rectangle (move $ upperLeft rect) (move $ lowerRight rect)
    where move p = let (x,y) = toCoords p in fromCoords (x + dx, y + dy)

-- | Given a rectangle $R_1$ in XY space and a rectangle $R_2$ in UV space,
--   make the affine transformation which maps $R_1$ to $R_2$.
convertRect :: (Planar xy, Planar uv) => Rectangle xy -> Rectangle uv -> xy -> uv
convertRect xyRect uvRect xy = fromCoords (u,v)
    where (x,y)   = toCoords xy
          (x0,y0) = toCoords $ upperLeft  xyRect
          (x1,y1) = toCoords $ lowerRight xyRect
          (u0,v0) = toCoords $ upperLeft  uvRect
          (u1,v1) = toCoords $ lowerRight uvRect
          uxRatio = (u1 - u0) / (x1 - x0)
          vyRatio = (v1 - v0) / (y1 - y0)
          (u,v) = (u0 + uxRatio * (x - x0), v0 + vyRatio * (y - y0))

-- | Determine if two rectangles intersect.
intersectsRect :: Planar a => Rectangle a -> Rectangle a -> Bool
intersectsRect rect1 rect2 = l1 <= r2 && l2 <= r1 && t1 <= b2 && t2 <= b1
    where (l1, t1) = toCoords $ upperLeft  rect1
          (r1, b1) = toCoords $ lowerRight rect1
          (l2, t2) = toCoords $ upperLeft  rect2
          (r2, b2) = toCoords $ lowerRight rect2

-- | Get the width and height of a rectangle.
dimensions :: Planar a => Rectangle a -> (Double, Double)
dimensions r = (x1 - x0, y1 - y0)
    where (x0,y0) = toCoords $ upperLeft r
          (x1,y1) = toCoords $ lowerRight r

instance Planar C where
    toCoords = coords
    fromCoords = uncurry complex

instance Planar R2 where
    toCoords (R2 x y) = (x,y)
    fromCoords (x,y) = R2 x y

newtype Viewport = Viewport (Int,Int) deriving (Eq,Ord,Show)

instance Planar Viewport where
    toCoords (Viewport (x,y)) = (fromIntegral x, fromIntegral y)
    fromCoords (x,y) = Viewport (floor x, floor y)

-- | Get a list of the four vertices of the rectangle, in 
--   clockwise order from the upper-left corner.
rectPoints :: Planar a
           => Rectangle a
           -> [a]
rectPoints r = [fromCoords (x0,y0), fromCoords (x1,y0), fromCoords (x1,y1), fromCoords (x0,y1)]
    where (x0,y0) = toCoords $ upperLeft r
          (x1,y1) = toCoords $ lowerRight r
