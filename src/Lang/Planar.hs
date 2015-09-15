
module Lang.Planar ( Planar (..)
                   , R
                   , Rectangle()
                   , rectangle
                   , upperLeft
                   , lowerRight
                   , translateRect
                   , convertRect
                   , intersectsRect
                   , dimensions
                   ) where

import Lang.Numbers

class Planar a where
    toCoords :: a -> (Double,Double)
    fromCoords :: (Double,Double) -> a

data Rectangle a = Rectangle { _upperLeft :: a, _lowerRight :: a }

upperLeft :: Rectangle a -> a
upperLeft = _upperLeft

lowerRight :: Rectangle a -> a
lowerRight = _lowerRight

rectangle :: Planar a => a -> a -> Rectangle a

rectangle p p' = Rectangle { _upperLeft  = fromCoords $ (min x x', min y y')
                           , _lowerRight = fromCoords $ (max x x', max y y')
                           }
    where (x , y ) = toCoords p
          (x', y') = toCoords p'

translateRect :: Planar a => (Double, Double) -> Rectangle a -> Rectangle a

translateRect (dx,dy) rect = rectangle (move $ upperLeft rect) (move $ lowerRight rect)
    where move p = let (x,y) = toCoords p in fromCoords (x + dx, y + dy)

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

intersectsRect :: Planar a => Rectangle a -> Rectangle a -> Bool

intersectsRect rect1 rect2 = l1 <= r2 && l2 <= r1 && t1 <= b2 && t2 <= b1
    where (l1, t1) = toCoords $ upperLeft  rect1
          (r1, b1) = toCoords $ lowerRight rect1
          (l2, t2) = toCoords $ upperLeft  rect2
          (r2, b2) = toCoords $ lowerRight rect2

dimensions :: Planar a => Rectangle a -> (Double, Double)
dimensions r = (x1 - x0, y1 - y0)
    where (x0,y0) = toCoords $ upperLeft r
          (x1,y1) = toCoords $ lowerRight r

instance Planar C where
    toCoords = coords
    fromCoords = uncurry complex