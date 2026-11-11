{- |
Module       : Actor.Viewer.Clip
Description  : Cutting a tool's drawings down to the part near the view

A tool can draw out to a point very far from the view: the next step of
an escaping orbit, or any point a little way off at a deep zoom, where
the distance in pixels is the distance in the model over the size of a
pixel. Neither frontend's drawing library copes with that. Rasterific
(the headless overlay, "Actor.Viewer.Overlay") works in single
precision, and from about 10^7 pixels away stroking doesn't finish, or
draws the wrong pixels; wxWidgets takes 32-bit integer coordinates, which
wrap around past about 2.1 * 10^9 pixels, sending a line off the other
way.

So both frontends work out where a shape is in pixels in double
precision, and draw a shape that stays 'near' the view as they always
have, while one reaching further is first cut down to the part of it
near the view, with the functions here.
-}
module Actor.Viewer.Clip
  ( Px
  , Box(..)
  , near
  , cutBox
  , finitePx
  , clipSegment
  , clipPolyline
  , clipPolygon
  , ellipseNear
  , boxCorners
  ) where

-- | A point of the image, in pixels (in double precision).
type Px = (Double, Double)

-- | A rectangle of pixel space: left, top, right, bottom.
data Box = Box !Double !Double !Double !Double

grow :: Double -> Box -> Box
grow m (Box x0 y0 x1 y1) = Box (x0 - m) (y0 - m) (x1 + m) (y1 + m)

inBox :: Box -> Px -> Bool
inBox (Box x0 y0 x1 y1) (x, y) = x >= x0 && x <= x1 && y >= y0 && y <= y1

-- | How far outside the view a shape can reach and still be drawn as it
-- is, by either frontend.
nearDistance :: Double
nearDistance = 1e4

-- | Whether points are near enough to the view (given as a box of pixels)
-- to draw as they are.
near :: Box -> [Px] -> Bool
near image = all (inBox (grow nearDistance image))

-- | Where a shape is cut off: far enough outside the image that a 2-pixel
-- stroke (and its round caps, and its antialiasing) along the cut doesn't
-- show.
cutBox :: Box -> Box
cutBox = grow 4

finitePx :: Px -> Bool
finitePx (x, y) = not (isNaN x || isInfinite x || isNaN y || isInfinite y)

-- | The part of a segment in a box, if any (by Cohen and Sutherland's
-- method). An end outside the box is moved onto the edge it's past, worked
-- out from the segment's other end: so a segment from a point in the box
-- out to a very distant one keeps its direction, and one along a row or
-- column of pixels stays on it, however far away its ends are.
clipSegment :: Box -> Px -> Px -> Maybe (Px, Px)
clipSegment (Box x0 y0 x1 y1) = go (8 :: Int)
  where
    outside (x, y) = (x < x0, x > x1, y < y0, y > y1)
    none (a, b, c, d) = not (a || b || c || d)
    shared (a, b, c, d) (a', b', c', d') = (a && a') || (b && b') || (c && c') || (d && d')
    go n p q
      | none op && none oq = Just (p, q)
      | shared op oq || n == 0 = Nothing
      | not (none op) = go (n - 1) (onEdge p q) q
      | otherwise = go (n - 1) p (onEdge q p)
      where
        op = outside p
        oq = outside q
    -- (p is outside the box, and q isn't past the same edge)
    onEdge (px, py) (qx, qy)
      | px < x0 = (x0, alongX x0)
      | px > x1 = (x1, alongX x1)
      | py < y0 = (alongY y0, y0)
      | otherwise = (alongY y1, y1)
      where
        alongX c = qy + (c - qx) * (py - qy) / (px - qx)
        alongY c = qx + (c - qy) * (px - qx) / (py - qy)

-- | The parts of a polyline in a box, each a polyline of its own.
clipPolyline :: Box -> [Px] -> [[Px]]
clipPolyline box pts =
  filter ((>= 2) . length) (map reverse (go [] (zip pts (drop 1 pts))))
  where
    -- (the run being built, backwards, then the finished ones)
    go run [] = [run]
    go run ((p, q) : rest) = case clipSegment box p q of
      Nothing -> run : go [] rest
      Just (p', q')
        | p' == p, (end : _) <- run, end == p -> go (q' : run) rest
        | otherwise -> run : go [q', p'] rest

-- | The part of a polygon in a box (by Sutherland and Hodgman's method).
clipPolygon :: Box -> [Px] -> [Px]
clipPolygon (Box x0 y0 x1 y1) =
    against (\(x, _) -> x >= x0) (atX x0)
  . against (\(x, _) -> x <= x1) (atX x1)
  . against (\(_, y) -> y >= y0) (atY y0)
  . against (\(_, y) -> y <= y1) (atY y1)
  where
    against inside cross poly = concat (zipWith edge poly (drop 1 poly ++ take 1 poly))
      where
        edge a b
          | inside b = [ cross a b | not (inside a) ] ++ [b]
          | inside a = [cross a b]
          | otherwise = []
    atX c (ax, ay) (bx, by) = (c, ay + (c - ax) * (by - ay) / (bx - ax))
    atY c (ax, ay) (bx, by) = (ax + (c - ay) * (bx - ax) / (by - ay), c)

-- | An ellipse (center, and radii in pixels) far from the image, or too
-- big for it: what of it to draw in a box. 'Nothing' if it can't reach
-- the box; @Just []@ if the box is all inside it; otherwise, points along
-- the part of it facing the box (all of it, if its center is in the box),
-- close enough together (within a tenth of a pixel of the curve) to join
-- up. Cutting those down to the box is left to 'clipPolyline' and
-- 'clipPolygon'.
ellipseNear :: Box -> Px -> Double -> Double -> Maybe [Px]
ellipseNear box@(Box x0 y0 x1 y1) (cx, cy) rx ry
  | x1 < cx - rx || x0 > cx + rx || y1 < cy - ry || y0 > cy + ry = Nothing
  | all inside corners = Just []
  | inBox box (cx, cy) = Just (arcFrom 0 (2 * pi))
  | otherwise = Just (arcFrom lo hi)
  where
    corners = [(x0, y0), (x1, y0), (x1, y1), (x0, y1)]
    unit (x, y) = ((x - cx) / rx, (y - cy) / ry)
    inside p = let (u, v) = unit p in u * u + v * v <= 1
    -- The box doesn't hold the center, so, seen from the center, it lies
    -- in a sector less than a half-turn wide: the sector its corners span.
    mid = let (u, v) = unit ((x0 + x1) / 2, (y0 + y1) / 2) in atan2 v u
    turn a = a - 2 * pi * fromIntegral (round (a / (2 * pi)) :: Int)
    offsets = [ turn (atan2 v u - mid) | c <- corners, let (u, v) = unit c ]
    (lo, hi) = (mid + minimum offsets, mid + maximum offsets)
    arcFrom a b =
      let n = min 100000 (ceiling ((b - a) / sqrt (0.8 / max rx ry)) + 1) :: Int
      in [ (cx + rx * cos t, cy + ry * sin t)
         | k <- [0 .. n], let t = a + (b - a) * fromIntegral k / fromIntegral n ]

boxCorners :: Box -> [Px]
boxCorners (Box x0 y0 x1 y1) = [(x0, y0), (x1, y0), (x1, y1), (x0, y1)]
