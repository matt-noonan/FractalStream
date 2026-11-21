{- |
Module      : Actor.Viewer.Sphere
Description : The geometry of a viewer that shows the Riemann sphere

A sphere viewer shows the Riemann sphere as a globe: the unit sphere,
seen from outside, from far away (an orthographic view). The screen is
measured in units of the sphere's radius, with the globe's center at
the origin, @x@ to the right and @y@ up; the viewer looks down the @-z@
axis, so the visible hemisphere is the one with @z > 0@ (in the
viewer's own frame).

A point of the sphere is a complex number by stereographic projection
from the south pole: @(X, Y, Z)@ is @(X + iY) / (1 + Z)@, so the north
pole is 0, the south pole is ∞, and the equator is the unit circle.
The pair @[X + iY : 1 + Z]@ is the point's homogeneous coordinates,
which stay finite everywhere (a viewer's code sees only the quotient,
for now, which loses precision near ∞).

The globe's orientation is a rotation taking the viewer's frame to the
sphere's: with no rotation, 0 faces the viewer, with the real axis to
the right and the imaginary axis up (as on the plane), and ∞ is
directly behind.
-}
module Actor.Viewer.Sphere
  ( -- * Rotations
    Rotation(..)
  , noRotation
  , rotationMatrix
  , rotateBy
  , unrotateBy
  , compose
  , normalizeRotation
  , rotationFacing
  , trackballRotation
  , slerp
    -- * Views: Möbius maps
  , Globe
  , noGlobe
  , rotationGlobe
  , composeGlobe
  , inverseGlobe
  , globeFacing
  , globeFacingRolled
  , globeCenter
  , globeZoom
  , globeTurn
  , withGlobeZoom
  , globeEntries
  , globeFromEntries
  , trackballGlobe
  , zoomGlobeAbout
  , slerpGlobe
  , minGlobeZoom
  , maxGlobeZoom
  , clampGlobeZoom
  , globeMobius
  , globeScreenPixelSize
    -- * Points
  , Vec3
  , screenToSphere
  , sphereToScreen
  , sphereToScreenWithin
  , globeReprojection
  , sphereToPlane
  , planeToSphere
  , screenToPlane
  , screenToHom
  , farAway
  , planeToScreen
    -- * How model points reach the screen
  , Projection(..)
  , ScreenView(..)
  , projectToScreen
  , visibleRuns
  , sampleSegment
  , sampleCircle
    -- * Drawing at homogeneous points
  , HPoint
  , homPlane
  , homToScreen
  , visibleRunsH
  , greatArc
  , sphereCircle
  , hyperbolicCircle
  , lineOnScreen
  , circleOnScreen
    -- * Defaults
  , defaultSpherePixelSize
  , sphereBackground
    -- * A viewer's code, on the sphere
  , sphereViewName
  , globeMobiusNames
  , sphereShadingName
  , sphereShading
  , sphereLight
  , globeLight
  , sphereViewEntries
  , onTheSphere
  ) where

import Actor.Viewer.Disk (Isometry, HyperbolicModel, HyperbolicDisplay, modelToScreenH, geodesicPoints, modelToDisk, diskToModel, applyMobius)
import Data.Color (Color, rgbToColor, black)
import Data.DynamicValue (Variable, newVariable)
import FractalStream.Settings (defaultSphereShading)
import System.IO.Unsafe (unsafePerformIO)
import Data.Complex (magnitude, conjugate, realPart, imagPart, phase, cis)
import Language.Type
import Language.Parser.SourceRange (SourceRange(..))
import Language.Code.Syntax
import Language.Value.Syntax
import Language.Value.Builtins (sphereViewMarker)

-- | A rotation, as a unit quaternion @w + x i + y j + z k@.
data Rotation = Rotation !Double !Double !Double !Double
  deriving (Eq, Show)

type Vec3 = (Double, Double, Double)

noRotation :: Rotation
noRotation = Rotation 1 0 0 0

normalizeRotation :: Rotation -> Rotation
normalizeRotation (Rotation w x y z)
  | n == 0 || isNaN n = noRotation
  | otherwise = Rotation (w / n) (x / n) (y / n) (z / n)
  where n = sqrt (w * w + x * x + y * y + z * z)

-- | The rotation's matrix, by rows.
rotationMatrix :: Rotation -> (Vec3, Vec3, Vec3)
rotationMatrix (Rotation w x y z) =
  ( (1 - 2 * (y * y + z * z), 2 * (x * y - w * z), 2 * (x * z + w * y))
  , (2 * (x * y + w * z), 1 - 2 * (x * x + z * z), 2 * (y * z - w * x))
  , (2 * (x * z - w * y), 2 * (y * z + w * x), 1 - 2 * (x * x + y * y)) )

-- | Rotate a vector: from the viewer's frame to the sphere's.
rotateBy :: Rotation -> Vec3 -> Vec3
rotateBy r (a, b, c) =
  let ((m11, m12, m13), (m21, m22, m23), (m31, m32, m33)) = rotationMatrix r
  in (m11 * a + m12 * b + m13 * c, m21 * a + m22 * b + m23 * c, m31 * a + m32 * b + m33 * c)

-- | The inverse rotation of a vector: from the sphere's frame to the
-- viewer's.
unrotateBy :: Rotation -> Vec3 -> Vec3
unrotateBy (Rotation w x y z) = rotateBy (Rotation w (-x) (-y) (-z))

-- | @compose a b@ rotates by @b@, then by @a@.
compose :: Rotation -> Rotation -> Rotation
compose (Rotation w1 x1 y1 z1) (Rotation w2 x2 y2 z2) = normalizeRotation $ Rotation
  (w1 * w2 - x1 * x2 - y1 * y2 - z1 * z2)
  (w1 * x2 + x1 * w2 + y1 * z2 - z1 * y2)
  (w1 * y2 - x1 * z2 + y1 * w2 + z1 * x2)
  (w1 * z2 + x1 * y2 - y1 * x2 + z1 * w2)

-- | The shortest rotation taking one unit vector to another.
rotationBetween :: Vec3 -> Vec3 -> Rotation
rotationBetween u@(ux, uy, uz) v@(vx, vy, vz)
  | d < -0.999999 =
      -- opposite: half a turn about any axis perpendicular to u
      let (ax, ay, az) = normalize (cross u (if abs ux < 0.9 then (1, 0, 0) else (0, 1, 0)))
      in Rotation 0 ax ay az
  | otherwise = normalizeRotation (Rotation (1 + d) cx cy cz)
  where
    d = ux * vx + uy * vy + uz * vz
    (cx, cy, cz) = cross u v

cross :: Vec3 -> Vec3 -> Vec3
cross (a, b, c) (d, e, f) = (b * f - c * e, c * d - a * f, a * e - b * d)

normalize :: Vec3 -> Vec3
normalize (a, b, c) = let n = sqrt (a * a + b * b + c * c) in (a / n, b / n, c / n)

-- | The orientation in which the point @z@ faces the viewer: the
-- shortest rotation from the one where 0 does.
--
-- It's worked out from @z@ itself, not from its point of the sphere, so
-- that it's exact however near ∞ @z@ is: the turn is by the angle θ with
-- @tan(θ/2) = |z|@, about the axis @(-y, x, 0)@.
rotationFacing :: (Double, Double) -> Rotation
rotationFacing (a, b)
  | isInfinite a || isInfinite b = Rotation 0 0 1 0
  | otherwise =
      -- (scaled by the larger of |a|, |b| and 1, so that nothing overflows)
      let m = maximum [1, abs a, abs b]
          (a', b', one) = (a / m, b / m, 1 / m)
          n = sqrt (one * one + a' * a' + b' * b')
      in Rotation (one / n) (negate b' / n) (a' / n) 0

-- | The orientation after dragging the globe, trackball-style, from one
-- screen point to another (in units of the sphere's radius): the point
-- of the sphere under the first stays under the pointer. A point off the
-- globe counts as on its rim.
trackballRotation :: Rotation -> (Double, Double) -> (Double, Double) -> Rotation
trackballRotation r from to =
  let lift (x, y) = let d = x * x + y * y
                    in if d >= 1 then normalize (x, y, 0) else (x, y, sqrt (1 - d))
  in compose r (rotationBetween (lift to) (lift from))

-- | Spherical interpolation, from @a@ (at 0) to @b@ (at 1), the shorter
-- way round.
slerp :: Rotation -> Rotation -> Double -> Rotation
slerp (Rotation w1 x1 y1 z1) (Rotation w2' x2' y2' z2') t
  | d > 0.9995 = mix (1 - t) t
  | otherwise =
      let th = acos d
      in mix (sin ((1 - t) * th) / sin th) (sin (t * th) / sin th)
  where
    d0 = w1 * w2' + x1 * x2' + y1 * y2' + z1 * z2'
    (sign, d) = if d0 < 0 then (-1, -d0) else (1, d0)
    mix s1 s2 = normalizeRotation $ Rotation
      (s1 * w1 + s2 * sign * w2') (s1 * x1 + s2 * sign * x2')
      (s1 * y1 + s2 * sign * y2') (s1 * z1 + s2 * sign * z2')

------------------------------------------------------------------------
-- Views: Möbius maps

-- | A sphere viewer's view: a Möbius map @M@, by its matrix @(a, b, c,
-- d)@ (with determinant 1), taking a point of the globe in the viewer's
-- frame, by its homogeneous stereographic coordinates @[u : v]@ (@w = u /
-- v@, 0 facing the viewer), to the point of the sphere it shows: @[a u +
-- b v : c u + d v]@. Navigating composes on the globe's side, as the globe
-- is seen: dragging turns the globe rigidly (@M ∘ R@, for the rotation
-- R), and zooming dilates it about the point under the pointer (@M ∘ D@,
-- for a dilation fixing that point of the globe). Unzoomed, @M@ is a
-- rotation, and the globe shows the front hemisphere as it is; zoomed in,
-- the middle of the zoom is magnified, the rest of the sphere squeezed
-- towards the rim; zoomed out, it shows more than a hemisphere.
data Globe = Globe !(Complex Double) !(Complex Double) !(Complex Double) !(Complex Double)
  deriving (Eq, Show)

-- | The matrix of a Möbius map, scaled to determinant 1.
mobiusOf :: Complex Double -> Complex Double -> Complex Double -> Complex Double -> Globe
mobiusOf a b c d = let s = sqrt (a * d - b * c) in Globe (a / s) (b / s) (c / s) (d / s)

-- | @M ∘ N@: first @N@, then @M@.
composeGlobe :: Globe -> Globe -> Globe
composeGlobe (Globe a b c d) (Globe e f g h) = mobiusOf (a * e + b * g) (a * f + b * h) (c * e + d * g) (c * f + d * h)

inverseGlobe :: Globe -> Globe
inverseGlobe (Globe a b c d) = Globe d (negate b) (negate c) a

applyGlobe :: Globe -> (Complex Double, Complex Double) -> (Complex Double, Complex Double)
applyGlobe (Globe a b c d) (u, v) = (a * u + b * v, c * u + d * v)

noGlobe :: Globe
noGlobe = Globe 1 0 0 1

-- | A rotation, as the Möbius map it is (found from where it takes 0, ∞
-- and 1).
rotationGlobe :: Rotation -> Globe
rotationGlobe r =
  let (p0a, p0b) = vecToHom (rotateBy r (0, 0, 1))
      (pia, pib) = vecToHom (rotateBy r (0, 0, -1))
      (p1a, p1b) = vecToHom (rotateBy r (1, 0, 0))
      k = (p0b * p1a - p0a * p1b) / (pia * p1b - pib * p1a)
  in mobiusOf (k * pia) p0a (k * pib) p0b

-- | The dilation @w -> f w@ (about 0 and ∞, the point facing the viewer
-- and the one behind).
dilationGlobe :: Double -> Globe
dilationGlobe f = mobiusOf (f :+ 0) 0 0 1

-- | A point of the sphere's homogeneous coordinates, computed without
-- cancellation on either half.
vecToHom :: Vec3 -> (Complex Double, Complex Double)
vecToHom (x, y, z)
  | z >= 0 = (x :+ y, (1 + z) :+ 0)
  | otherwise = ((1 - z) :+ 0, x :+ negate y)

-- | The point of the sphere with these homogeneous coordinates.
homToVec :: (Complex Double, Complex Double) -> Vec3
homToVec (a, b) =
  let n = magnitude a ^ (2 :: Int) + magnitude b ^ (2 :: Int)
      p = a * conjugate b
  in (2 * realPart p / n, 2 * imagPart p / n, (magnitude b ^ (2 :: Int) - magnitude a ^ (2 :: Int)) / n)

-- | The view facing the point @z@, unzoomed.
globeFacing :: (Double, Double) -> Globe
globeFacing z = rotationGlobe (rotationFacing z)

-- | The view facing the point @z@, turned by an angle (in radians,
-- counterclockwise) about the line of sight, at a zoom: a pixel in the
-- middle shows a piece of the sphere @zoom@ times its size unzoomed.
globeFacingRolled :: (Double, Double) -> Double -> Double -> Globe
globeFacingRolled z angle zoom =
  rotationGlobe (rotationFacing z)
    `composeGlobe` mobiusOf (cis (angle / 2)) 0 0 (cis (negate angle / 2))
    `composeGlobe` dilationGlobe zoom

-- | The point in the middle of a view (as a complex number: infinite at
-- ∞), exact however near ∞ it is: the view takes 0 to @b / d@.
globeCenter :: Globe -> (Double, Double)
globeCenter (Globe _ b _ d) = if d == 0 then (1 / 0, 0) else let x :+ y = b / d in (x, y)

-- | How a view is zoomed and turned in its middle, beyond facing the
-- point there: @N'(0)@ for @N@ the view after the rotation back from that
-- point (which leaves 0 where it is), its size the zoom and its argument
-- the turn.
globeLocal :: Globe -> Complex Double
globeLocal g =
  let Globe a b c d = inverseGlobe (globeFacing (globeCenter g)) `composeGlobe` g
      -- (N(w) = (a w + b) / (c w + d), with b = 0: N'(0) = a / d)
  in if d == 0 then 0 / 0 else (a * d - b * c) / (d * d)

-- | How far a view is zoomed in its middle: a pixel there shows a piece of
-- the sphere this many times the size it would unzoomed.
globeZoom :: Globe -> Double
globeZoom = magnitude . globeLocal

-- | How far a view is turned (counterclockwise, in radians) about the
-- line of sight, beyond facing the point in its middle.
globeTurn :: Globe -> Double
globeTurn = phase . globeLocal

-- | How far a view can zoom in, and out (see 'Globe').
minGlobeZoom, maxGlobeZoom :: Double
minGlobeZoom = 1e-14
maxGlobeZoom = 100

clampGlobeZoom :: Double -> Double
clampGlobeZoom l = max minGlobeZoom (min maxGlobeZoom l)

-- | The view with another zoom in its middle, facing the same point,
-- turned the same way.
withGlobeZoom :: Double -> Globe -> Globe
withGlobeZoom zoom g = globeFacingRolled (globeCenter g) (globeTurn g) (clampGlobeZoom zoom)

-- | A view's matrix, by its entries' real and imaginary parts (as a
-- project file saves it).
globeEntries :: Globe -> [Double]
globeEntries (Globe a b c d) = concat [ [realPart x, imagPart x] | x <- [a, b, c, d] ]

-- | A view from 'globeEntries', if they make one.
globeFromEntries :: [Double] -> Maybe Globe
globeFromEntries = \case
  [ar, ai, br, bi, cr, ci, dr, di] ->
    let det = (ar :+ ai) * (dr :+ di) - (br :+ bi) * (cr :+ ci)
    in if magnitude det > 0 && not (isNaN (magnitude det))
         then Just (mobiusOf (ar :+ ai) (br :+ bi) (cr :+ ci) (dr :+ di)) else Nothing
  _ -> Nothing

-- | The point of the sphere (in the sphere's frame) under a screen
-- point, if it's on the globe.
screenToSphere :: Globe -> (Double, Double) -> Maybe Vec3
screenToSphere g (x, y)
  | d > 1 = Nothing
  | otherwise = Just (onGlobe g (x, y, sqrt (1 - d)))
  where d = x * x + y * y

-- | The point of the sphere a point of the globe (in the viewer's frame)
-- shows.
onGlobe :: Globe -> Vec3 -> Vec3
onGlobe g = homToVec . applyGlobe g . vecToHom

-- | Where a point of the sphere is on the screen, kept within a radius
-- (less than 1), and whether it's on the visible side: a point on the far
-- side, or too near the rim, is taken to the nearest place at that
-- radius.
sphereToScreenWithin :: Double -> Globe -> Vec3 -> ((Double, Double), Bool)
sphereToScreenWithin rmax g p =
  let (x, y, z) = homToVec (applyGlobe (inverseGlobe g) (vecToHom p))
      r = sqrt (x * x + y * y)
      pulled = if r == 0 then (rmax, 0) else (rmax * x / r, rmax * y / r)
  in (if z >= 0 && r <= rmax then (x, y) else pulled, z >= 0)

-- | For moving a picture of the globe from one view to another: given the
-- old view, the new one and a radius (less than 1), a map from a screen
-- point on the new view's globe to where the same point of the sphere is
-- on the old view's screen, as 'sphereToScreenWithin' takes it (kept
-- within the radius), with whether the old view showed it. It's applied
-- to every pixel, so the views are composed once, and each point worked
-- out in plain numbers.
globeReprojection :: Globe -> Globe -> Double -> (Double -> Double -> (Double, Double, Bool))
globeReprojection old new rmax =
  let Globe (ar :+ ai) (br :+ bi) (cr :+ ci) (dr :+ di) = inverseGlobe old `composeGlobe` new
  in \ !x !y ->
    let !s = sqrt (max 0 (1 - x * x - y * y))
        !v = 1 + s
        -- (A, B) = N (x + iy, v)
        !aRe = ar * x - ai * y + br * v
        !aIm = ar * y + ai * x + bi * v
        !bRe = cr * x - ci * y + dr * v
        !bIm = cr * y + ci * x + di * v
        !na = aRe * aRe + aIm * aIm
        !nb = bRe * bRe + bIm * bIm
        !n = na + nb
        -- (A conj(B))
        !pRe = aRe * bRe + aIm * bIm
        !pIm = aIm * bRe - aRe * bIm
        !ox = 2 * pRe / n
        !oy = 2 * pIm / n
        !seen = nb >= na
        !r2 = ox * ox + oy * oy
    in if seen && r2 <= rmax * rmax then (ox, oy, True)
       else let !r = sqrt r2
            in if r == 0 then (rmax, 0, seen) else (rmax * ox / r, rmax * oy / r, seen)
{-# INLINE globeReprojection #-}

-- | Where a point of the sphere is on the screen, if it's on the
-- visible side.
sphereToScreen :: Globe -> Vec3 -> Maybe (Double, Double)
sphereToScreen g p =
  let (x, y, z) = homToVec (applyGlobe (inverseGlobe g) (vecToHom p))
  in if z < 0 then Nothing else Just (x, y)

-- | A point of the sphere as a complex number, @(X + iY) / (1 + Z)@ (as
-- a pair of reals; the south pole comes out infinite or NaN).
sphereToPlane :: Vec3 -> (Double, Double)
sphereToPlane (x, y, z) = let d = 1 + z in (x / d, y / d)

-- | The point of the sphere for a complex number. An infinite one is the
-- south pole.
planeToSphere :: (Double, Double) -> Vec3
planeToSphere (a, b)
  | isInfinite a || isInfinite b = (0, 0, -1)
  | otherwise =
      let m = a * a + b * b
      in (2 * a / (1 + m), 2 * b / (1 + m), (1 - m) / (1 + m))

-- | The complex number under a screen point, if it's on the globe:
-- worked out from the view's Möbius map, as the kernel does (@A / B@ for
-- @[A : B] = M [x + iy : 1 + s]@), not through the sphere's 3-D points,
-- so that it keeps its precision however near ∞ it is. ∞ itself, which
-- a complex number can't be, is 'farAway'.
screenToPlane :: Globe -> (Double, Double) -> Maybe (Double, Double)
screenToPlane g = fmap homToPlane . screenToHom g

-- | The point of the sphere under a screen point, if it's on the globe,
-- in homogeneous coordinates: exact everywhere, ∞ included (as @[A : B]
-- = M [x + iy : 1 + s]@, which the kernel works out too). A tool's
-- events on a sphere viewer are given these (see 'Actor.Event.Point').
screenToHom :: Globe -> (Double, Double) -> Maybe (Complex Double, Complex Double)
screenToHom g (x, y)
  | d > 1 = Nothing
  | otherwise = Just (applyGlobe g (x :+ y, (1 + sqrt (1 - d)) :+ 0))
  where d = x * x + y * y

-- | The complex number with these homogeneous coordinates; for ∞,
-- 'farAway'.
homToPlane :: (Complex Double, Complex Double) -> (Double, Double)
homToPlane (a, b)
  | magnitude b * farAway <= magnitude a = (farAway, 0)
  | otherwise = let u :+ v = a / b in (u, v)

-- | The complex number that stands in for ∞ where a point has to be a
-- complex number (a click on ∞, say): so far out that no view can tell
-- it from ∞ (a point of the sphere 10⁻¹⁵⁰ from it), and its square still
-- a finite number, so that working with it on Ĉ goes as it would at ∞.
farAway :: Double
farAway = 1e150

-- | Where a complex number is on the screen, if it's on the visible side
-- (an infinite one, or one beyond 'farAway', is ∞): through the view's
-- Möbius map, as 'screenToPlane' works, keeping its precision near ∞.
planeToScreen :: Globe -> (Double, Double) -> Maybe (Double, Double)
planeToScreen g (u, v) =
  let hom | isInfinite u || isInfinite v || u * u + v * v >= farAway * farAway = (1, 0)
          | otherwise = (u :+ v, 1)
      (x, y, z) = homToVec (applyGlobe (inverseGlobe g) hom)
  in if z < 0 then Nothing else Just (x, y)

-- | The view after dragging the globe from one screen point to another
-- (in units of the sphere's radius): the globe turned rigidly, as it's
-- seen, so that the point under the first is under the second (a point
-- off the globe counts as on its rim).
trackballGlobe :: Globe -> (Double, Double) -> (Double, Double) -> Globe
trackballGlobe g from to =
  let lift (x, y) = let d = x * x + y * y
                    in if d >= 1 then normalize (x, y, 0) else (x, y, sqrt (1 - d))
  in g `composeGlobe` rotationGlobe (rotationBetween (lift to) (lift from))

-- | The view zoomed by a factor (less than 1 zooms in) about the point of
-- the globe under a screen point, which stays where it is: a dilation of
-- the globe, as it's seen, fixing that point (and the one opposite). The
-- view stays as it is if that would zoom its middle too far either way.
zoomGlobeAbout :: Globe -> (Double, Double) -> Double -> Globe
zoomGlobeAbout g (x, y) f =
  let d2 = x * x + y * y
      q = if d2 >= 1 then normalize (x, y, 0) else (x, y, sqrt (1 - d2))
      t = rotationGlobe (rotationBetween (0, 0, 1) q)
      g' = g `composeGlobe` t `composeGlobe` dilationGlobe f `composeGlobe` inverseGlobe t
      z = globeZoom g'
  in if z >= minGlobeZoom && z <= maxGlobeZoom then g' else g

-- | Interpolate views, from one (at 0) to the other (at 1), along the
-- one-parameter group through @M₁⁻¹ M₂@.
slerpGlobe :: Globe -> Globe -> Double -> Globe
slerpGlobe g1 g2 t =
  let Globe a0 b0 c0 d0 = inverseGlobe g1 `composeGlobe` g2
      -- (the sign of the matrix with a trace of positive real part, for
      -- the shorter way)
      flipSign = realPart (a0 + d0) < 0
      sgn x = if flipSign then negate x else x
      (a, b, c, d) = (sgn a0, sgn b0, sgn c0, sgn d0)
      tr = a + d
      disc = sqrt (tr * tr - 4)
      m1 = (tr + disc) / 2
      m2 = (tr - disc) / 2
      tc = t :+ 0
      pow m = exp (tc * log m)
      n | magnitude disc < 1e-9 = Globe (1 + tc * (a - 1)) (tc * b) (tc * c) (1 + tc * (d - 1))
        | otherwise =
            let p = (pow m1 - pow m2) / (m1 - m2)
                q = (m1 * pow m2 - m2 * pow m1) / (m1 - m2)
            in Globe (p * a + q) (p * b) (p * c) (p * d + q)
      Globe na nb nc nd = n
  in g1 `composeGlobe` mobiusOf na nb nc nd

-- | A view's Möbius map, @(a, b, c, d)@ (see 'Globe').
globeMobius :: Globe -> (Complex Double, Complex Double, Complex Double, Complex Double)
globeMobius (Globe a b c d) = (a, b, c, d)

-- | The size of a pixel (in units of the sphere's radius) that makes the
-- globe fill a window of the given size, with a small margin.
globeScreenPixelSize :: (Int, Int) -> Double
globeScreenPixelSize (w, h) = 2 / (0.92 * fromIntegral (max 1 (min w h)))

------------------------------------------------------------------------
-- How model points reach the screen

-- | How a viewer's model points are placed on its screen plane: as they
-- are (the plane), on the globe, with its orientation, or in the
-- hyperbolic plane, from its model, moved by its view, onto its display
-- (see "Actor.Viewer.Disk").
data Projection = FlatProjection | GlobeProjection Globe
                | HyperbolicProjection HyperbolicModel HyperbolicDisplay Isometry
  deriving (Eq, Show)

-- | Where a viewer's screen plane is: the point at the image's center,
-- the width and height of a pixel, and how model points get onto it.
-- (On the sphere, the screen plane is measured in units of the
-- sphere's radius, and its center is 0.)
data ScreenView = ScreenView
  { svCenter     :: (Double, Double)
  , svPixelSize  :: (Double, Double)
  , svProjection :: Projection
  , svCopies     :: [Projection]
    -- ^ the projections drawings are drawn with (see
    -- 'Actor.Viewer.overlayProjections'): 'svProjection', and with a group,
    -- one for each tile on the screen
  } deriving (Eq, Show)

-- | A model point's place on the screen plane, if it's visible.
projectToScreen :: Projection -> (Double, Double) -> Maybe (Double, Double)
projectToScreen = \case
  FlatProjection -> Just
  GlobeProjection g -> planeToScreen g
  HyperbolicProjection model display v -> modelToScreenH model display v

-- | The runs of a polyline (in model points) that are visible, on the
-- screen plane.
visibleRuns :: Projection -> [(Double, Double)] -> [[(Double, Double)]]
visibleRuns proj = splitRuns . map (projectToScreen proj)

-- | Points along a straight segment of the plane, ends included: on the
-- sphere, a segment is an arc, drawn as this many pieces.
sampleSegment :: Int -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
sampleSegment n (x1, y1) (x2, y2) =
  [ (x1 + t * (x2 - x1), y1 + t * (y2 - y1)) | k <- [0 .. n], let t = fromIntegral k / fromIntegral n ]

-- | Points around a circle of the plane, the first repeated at the end.
sampleCircle :: Int -> (Double, Double) -> Double -> [(Double, Double)]
sampleCircle n (cx, cy) r =
  [ (cx + r * cos a, cy + r * sin a) | k <- [0 .. n], let a = 2 * pi * fromIntegral k / fromIntegral n ]

------------------------------------------------------------------------
-- Drawing at homogeneous points

-- | A point a tool draws at, by its homogeneous coordinates @[a : b]@
-- (see 'Language.Draw.DrawSink'): on a sphere viewer, any point of the
-- sphere; on any other, @b@ is 1.
type HPoint = (Complex Double, Complex Double)

-- | The point of the plane a drawing's point is, off the sphere (where its
-- @b@ is 1).
homPlane :: HPoint -> (Double, Double)
homPlane (a, b) = let x :+ y = a / b in (x, y)

-- | A homogeneous point scaled so that its larger coordinate is 1 (so
-- that working with it can't overflow).
scaleHom :: HPoint -> HPoint
scaleHom (a, b) = let m = max (magnitude a) (magnitude b)
                  in if m == 0 || isInfinite m then (a, b) else (a / (m :+ 0), b / (m :+ 0))

-- | Where a drawing's point is on the screen plane, if it's visible: on the
-- globe, through the view's Möbius map, exact however near ∞.
homToScreen :: Projection -> HPoint -> Maybe (Double, Double)
homToScreen proj p = case proj of
  GlobeProjection g ->
    let (x, y, z) = homToVec (applyGlobe (inverseGlobe g) (scaleHom p))
    in if z < 0 then Nothing else Just (x, y)
  _ -> projectToScreen proj (homPlane p)

-- | The runs of a polyline (in drawing points) that are visible, on the
-- screen plane.
visibleRunsH :: Projection -> [HPoint] -> [[(Double, Double)]]
visibleRunsH proj = splitRuns . map (homToScreen proj)

splitRuns :: [Maybe (Double, Double)] -> [[(Double, Double)]]
splitRuns = go []
  where
    go run = \case
      [] -> finish run
      Just p : rest -> go (p : run) rest
      Nothing : rest -> finish run ++ go [] rest
    finish run = [ reverse run | length run >= 2 ]

-- | A rotation of the sphere taking 0 to a point (and ∞ to the point
-- opposite).
rotationTo :: HPoint -> Globe
rotationTo p = let (a, b) = scaleHom p
                   n = sqrt (magnitude a ^ (2 :: Int) + magnitude b ^ (2 :: Int)) :+ 0
               in Globe (conjugate b / n) (a / n) (negate (conjugate a) / n) (b / n)

-- | Points along the shorter arc of the great circle from one point of the
-- sphere to another, ends included, as this many pieces. Between opposite
-- points, every great circle through them is as short: this takes one.
greatArc :: Int -> HPoint -> HPoint -> [HPoint]
greatArc n p q =
  let r = rotationTo p
      (qa, qb) = scaleHom (applyGlobe (inverseGlobe r) (scaleHom q))
      -- (q, with p turned to 0: at an angle θ from it, in the direction u)
      theta = 2 * atan2 (magnitude qa) (magnitude qb)
      d = qa * conjugate qb
      u = if magnitude d == 0 then 1 else d / (magnitude d :+ 0)
  in [ applyGlobe r ((sin (t / 2) :+ 0) * u, cos (t / 2) :+ 0)
     | k <- [0 .. n], let t = theta * fromIntegral k / fromIntegral n ]

-- | Points around the circle of the sphere at a chordal distance from a
-- point (see @chordal@), the first repeated at the end.
sphereCircle :: Int -> HPoint -> Double -> [HPoint]
sphereCircle n c r =
  let rot = rotationTo c
      theta = 2 * asin (max 0 (min 1 (r / 2)))
  in [ applyGlobe rot ((sin (theta / 2) :+ 0) * cis phi, cos (theta / 2) :+ 0)
     | k <- [0 .. n], let phi = 2 * pi * fromIntegral k / fromIntegral n ]

-- | Points around the circle of the hyperbolic plane with a hyperbolic
-- radius about a point of a model (for the metric of curvature -1), the
-- first repeated at the end.
hyperbolicCircle :: HyperbolicModel -> Int -> (Double, Double) -> Double -> [(Double, Double)]
hyperbolicCircle model n c r =
  let (cx, cy) = modelToDisk model c
      p = cx :+ cy
      t = tanh (r / 2) :+ 0
      -- (the circle about 0 of that radius, moved to p)
      move w = (w + p) / (1 + conjugate p * w)
      toModel w = let x :+ y = applyMobius (diskToModel model) w in (x, y)
  in [ toModel (move (t * cis phi)) | k <- [0 .. n], let phi = 2 * pi * fromIntegral k / fromIntegral n ]

-- | A tool's line from one point to another, as the viewer's geodesic
-- between them, in its visible runs on the screen plane: a straight
-- segment in the plane, the shorter arc of a great circle on the sphere,
-- the hyperbolic geodesic in the disk or the half-plane.
lineOnScreen :: Projection -> HPoint -> HPoint -> [[(Double, Double)]]
lineOnScreen proj p q = case proj of
  FlatProjection -> visibleRuns proj [homPlane p, homPlane q]
  GlobeProjection _ -> visibleRunsH proj (greatArc 64 p q)
  HyperbolicProjection model _ _ -> visibleRuns proj (geodesicPoints model 64 (homPlane p) (homPlane q))

-- | A tool's circle about a point, with a radius in the viewer's own
-- metric (Euclidean in the plane, chordal on the sphere, hyperbolic in the
-- disk or the half-plane), in its visible runs on the screen plane, and
-- whether all of it is visible (so that it can be filled).
circleOnScreen :: Projection -> HPoint -> Double -> ([[(Double, Double)]], Bool)
circleOnScreen proj c r =
  let pts = case proj of
        FlatProjection -> map Just (sampleCircle 96 (homPlane c) r)
        GlobeProjection _ -> map (homToScreen proj) (sphereCircle 96 c r)
        HyperbolicProjection model _ _ -> map (projectToScreen proj) (hyperbolicCircle model 96 (homPlane c) r)
  in (splitRuns pts, all (maybe False (const True)) pts)

-- | The size of a pixel (in units of the sphere's radius) that a sphere
-- viewer starts with, unless its script says: the globe fills most of
-- a 600-pixel window.
defaultSpherePixelSize :: Double
defaultSpherePixelSize = 1 / 240

-- | The color off the globe.
sphereBackground :: Color
sphereBackground = rgbToColor (48, 48, 48)

------------------------------------------------------------------------
-- A viewer's code, on the sphere

-- | The name under which a complex viewer's code is told whether it's
-- showing the sphere (a truth value no script can name).
sphereViewName :: String
sphereViewName = sphereViewMarker

-- | The names under which a sphere viewer's code is given its view's
-- Möbius map, @a@, @b@, @c@ and @d@ (see 'globeMobius').
globeMobiusNames :: [String]
globeMobiusNames = [ "[internal] sphere view " ++ [k] | k <- "abcd" ]


-- | The name under which a complex viewer's code is told how strongly
-- to shade the globe, from 0 to 1 (see 'onTheSphere').
sphereShadingName :: String
sphereShadingName = "[internal] sphere shading"

-- | How strongly sphere viewers shade the globe, from 0 (not at all) to
-- 1: a live value, read by every complex viewer's code (like its
-- rotation, so changing it means drawing again, not compiling again).
-- The app sets it from its settings ("FractalStream.Settings"); until
-- then, and in @fsrun@, it's the settings' default.
sphereShading :: Variable Double
sphereShading = unsafePerformIO (newVariable (fromIntegral defaultSphereShading / 100))
{-# NOINLINE sphereShading #-}

-- | The direction the light comes from, in the viewer's frame (so it
-- stays put as the globe turns): the upper left, in front of the globe.
sphereLight :: Vec3
sphereLight = (-1 / n, 1 / n, 1.5 / n)
  where n = sqrt 4.25

-- | How the globe's shading scales a color at a point of the screen (in
-- the sphere's radii), for a shading strength from 0 to 1: the factor
-- 'onTheSphere' darkens the color by there.
globeLight :: Double -> (Double, Double) -> Double
globeLight shading (x, y) =
  let (lx, ly, lz) = sphereLight
      z = sqrt (max 0 (1 - x * x - y * y))
      light = (lx * x + ly * y + lz * z + 1) / 2
  in 1 - shading * (1 - light)

-- | What a complex viewer's code is given, besides its configuration, so
-- that it can be shown on the plane or on the sphere.
sphereViewEntries :: [(String, SomeType)]
sphereViewEntries = (sphereViewName, SomeType BooleanType)
                  : (sphereShadingName, SomeType RealType)
                  : [ (n, SomeType ComplexType) | n <- globeMobiusNames ]

-- | A sphere viewer's code (with coordinate @z@), wrapped so that @z@
-- is the point of the sphere under the pixel, a point of Ĉ (see
-- "Language.Code.Projective"), and a pixel off the globe gets the
-- background color:
--
-- > if x² + y² > 1:
-- >   color <- background
-- > else:
-- >   s := sqrt(1 - x² - y²)
-- >   u, v := x + y i, 1 + s           -- the globe's own point, [u : v]
-- >   A, B := a u + b v, c u + d v      -- the view's Möbius map
-- >   z : Ĉ := [A : B]
-- >   pixel_size <- dx (|u|² + |v|²) / (|A|² + |B|²)
-- >   body
-- >   light := ((x, y, s) · L + 1) / 2
-- >   color <- blend(1 - shading (1 - light), black, color)
--
-- On the visible side @s >= 0@, so @[u : v]@ is computed without
-- cancellation, and the Möbius map (the view's rotation after its zoom:
-- see 'globeMobius') takes it to the exact point of the sphere, ∞
-- included, however far the view is zoomed in. @pixel_size@ (when the
-- viewer names it) is a pixel's size on the sphere there: the screen's,
-- times how much the view's Möbius map stretches the sphere there (the
-- formula is that stretch, for a map of determinant 1). A @z@ the
-- viewer already has (a configuration value of Ĉ) is set, rather than
-- declared. For code that's already had its points of the sphere made
-- into pairs (see 'Language.Code.Typecheck.runExpandedCode'), @z@ is the
-- pair. The shading darkens the globe away from the light ('sphereLight',
-- @L@), by the viewer's @[internal] sphere shading@ ('sphereShading'): a
-- "half Lambert" light, which falls off gently and never goes all the way
-- to black on the visible side.
onTheSphere :: String -> Maybe String -> Bool -> Bool -> Stmt -> Stmt
onTheSphere z mpx declared expanded body =
  stmt (SIf offGlobe (stmt (SSet "color" background))
                     (stmt (SBlock (lift ++ [body] ++ shade))))
  where
    stmt = Stmt NoSourceRange
    e = Syn NoSourceRange
    var = e . EVar
    real d = e (ELit (show d) (SomeHaskellType RealType d))
    bin op a b = e (EBin op a b)
    sx = var "[internal] x"
    sy = var "[internal] y"
    d2 = bin OpAdd (bin OpMul sx sx) (bin OpMul sy sy)
    offGlobe = bin OpGt d2 (real 1)
    background = e (ELit "the background" (SomeHaskellType ColorType sphereBackground))
    i = e (ELit "i" (SomeHaskellType ComplexType (0 :+ 1)))
    s = "[internal] sphere.s"
    u = "[internal] sphere.u"
    v = "[internal] sphere.v"
    ca = "[internal] sphere.A"
    cb = "[internal] sphere.B"
    mobius k = var ("[internal] sphere view " ++ [k])
    (ma, mb, mc, md) = (mobius 'a', mobius 'b', mobius 'c', mobius 'd')
    complex x = e (ECast x ComplexT)
    point a b | expanded = e (EPair (complex a) (complex b))
              | otherwise = e (EHomogeneous [a, b])
    pointT = if expanded then Pair ComplexT ComplexT else ExtComplexT
    here = e (ECast (point (var ca) (var cb)) pointT)
    norm2 x = let m = e (EUn OpAbs x) in bin OpMul m m
    -- (a pixel's size on the sphere: the screen's, times the view's
    -- stretch there)
    stretch = bin OpDiv (bin OpAdd d2 (bin OpMul (var v) (var v)))
                        (bin OpAdd (norm2 (var ca)) (norm2 (var cb)))
    lift =
      [ stmt (SDecl s (Just RealT) (e (EApp "sqrt" (bin OpSub (real 1) d2))))
      , stmt (SDecl u (Just ComplexT) (bin OpAdd sx (bin OpMul sy i)))
      , stmt (SDecl v (Just RealT) (bin OpAdd (real 1) (var s)))
      , stmt (SDecl ca (Just ComplexT) (bin OpAdd (bin OpMul ma (var u)) (bin OpMul mb (var v))))
      , stmt (SDecl cb (Just ComplexT) (bin OpAdd (bin OpMul mc (var u)) (bin OpMul md (var v)))) ]
      ++ [ stmt (SSet px (bin OpMul (var "[internal] dx") stretch)) | Just px <- [mpx] ]
      ++ [ stmt (if declared then SDecl z (Just pointT) here else SSet z here) ]
    (lx, ly, lz) = sphereLight
    light = "[internal] sphere.light"
    shading = var sphereShadingName
    facing = foldr1 (bin OpAdd)
      [ bin OpMul (real lx) sx, bin OpMul (real ly) sy
      , bin OpMul (real lz) (var s) ]
    shade =
      [ stmt (SDecl light (Just RealT) (bin OpDiv (bin OpAdd facing (real 1)) (real 2)))
      , stmt (SSet "color" (e (EColor3 OpBlend
                                 (bin OpSub (real 1) (bin OpMul shading (bin OpSub (real 1) (var light))))
                                 (var "color")
                                 (e (ELit "black" (SomeHaskellType ColorType black)))))) ]
