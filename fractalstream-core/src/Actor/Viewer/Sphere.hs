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
  , rollOf
    -- * Views: a rotation and a zoom
  , Globe(..)
  , noGlobe
  , globeFacing
  , globeFacingRolled
  , globeCenter
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
  , sphereToPlane
  , planeToSphere
  , screenToPlane
  , planeToScreen
    -- * How model points reach the screen
  , Projection(..)
  , ScreenView(..)
  , projectToScreen
  , visibleRuns
  , sampleSegment
  , sampleGeodesic
  , sampleCircle
    -- * Defaults
  , defaultSpherePixelSize
  , sphereBackground
    -- * A viewer's code, on the sphere
  , sphereViewName
  , globeMobiusNames
  , sphereZoomName
  , sphereShadingName
  , sphereShading
  , sphereLight
  , sphereViewEntries
  , onTheSphere
  ) where

import Actor.Viewer.Disk (Isometry, HyperbolicModel, HyperbolicDisplay, modelToScreenH, geodesicPoints)
import Data.Color (Color, rgbToColor, black)
import Data.DynamicValue (Variable, newVariable)
import FractalStream.Settings (defaultSphereShading)
import System.IO.Unsafe (unsafePerformIO)
import Language.Type
import Language.Parser.SourceRange (SourceRange(..))
import Language.Code.Syntax
import Language.Value.Syntax

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
-- Views: a rotation and a zoom

-- | A sphere viewer's view: the globe's orientation, and its zoom, a
-- dilation @w -> λ w@ about the point facing the viewer (in the
-- stereographic coordinate @w@ of the viewer's frame, where that point is
-- 0). So the point under a pixel of the globe whose own point is @w@ is
-- the rotation of @λ w@: the Möbius map 'globeMobius'. At @λ = 1@ the
-- globe shows its front hemisphere as it is; a smaller @λ@ zooms in on the
-- point in the middle (the rest of the sphere squeezed towards the rim),
-- and a larger one shows more than a hemisphere.
data Globe = Globe { globeRotation :: !Rotation, globeZoom :: !Double }
  deriving (Eq, Show)

noGlobe :: Globe
noGlobe = Globe noRotation 1

-- | The view facing the point @z@, unzoomed.
globeFacing :: (Double, Double) -> Globe
globeFacing z = Globe (rotationFacing z) 1

-- | The view facing the point @z@, turned by an angle (in radians,
-- counterclockwise) about the line of sight, at a zoom.
globeFacingRolled :: (Double, Double) -> Double -> Double -> Globe
globeFacingRolled z angle = Globe (compose (rotationFacing z) (Rotation (cos (angle / 2)) 0 0 (sin (angle / 2))))

-- | The point in the middle of a view (as a complex number: infinite at
-- ∞).
-- It's found from the view's Möbius map ('globeMobius'), which takes 0
-- to @b / d@, so that it's exact however near ∞ it is.
globeCenter :: Globe -> (Double, Double)
globeCenter g =
  let (_, b, _, d) = globeMobius g
  in if d == 0 then (1 / 0, 0) else let x :+ y = b / d in (x, y)

-- | How far a rotation is turned about the line of sight, beyond the
-- shortest rotation facing the same point ('rotationFacing'): in radians.
rollOf :: Rotation -> Double
rollOf r =
  let facing = rotationFacing (globeCenter (Globe r 1))
      (x, y, _) = unrotateBy facing (rotateBy r (1, 0, 0))
  in atan2 y x

-- | How far a view can zoom in, and out (see 'Globe').
minGlobeZoom, maxGlobeZoom :: Double
minGlobeZoom = 1e-14
maxGlobeZoom = 100

clampGlobeZoom :: Double -> Double
clampGlobeZoom l = max minGlobeZoom (min maxGlobeZoom l)

-- | A point of the viewer's frame, dilated by @λ@ in its stereographic
-- coordinate.
dilate :: Double -> Vec3 -> Vec3
dilate l p@(x, y, z)
  | l == 1 = p
  | otherwise =
      let (a, b) = sphereToPlane (x, y, z)
          m = l * l * (a * a + b * b)
      in if isInfinite a || isInfinite b || isNaN m then (0, 0, -1)
         else (2 * l * a / (1 + m), 2 * l * b / (1 + m), (1 - m) / (1 + m))

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
onGlobe (Globe r l) = rotateBy r . dilate l

-- | Where a point of the sphere is on the screen, if it's on the
-- visible side.
sphereToScreen :: Globe -> Vec3 -> Maybe (Double, Double)
sphereToScreen (Globe r l) p =
  let (x, y, z) = dilate (1 / l) (unrotateBy r p)
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

-- | The complex number under a screen point, if it's on the globe.
screenToPlane :: Globe -> (Double, Double) -> Maybe (Double, Double)
screenToPlane g = fmap sphereToPlane . screenToSphere g

-- | Where a complex number is on the screen, if it's on the visible side.
planeToScreen :: Globe -> (Double, Double) -> Maybe (Double, Double)
planeToScreen g = sphereToScreen g . planeToSphere

-- | The view after dragging the globe from one screen point to another
-- (in units of the sphere's radius): turned so that the point of the
-- sphere under the first is under the second (a point off the globe
-- counts as on its rim). Zoomed in, this pans.
trackballGlobe :: Globe -> (Double, Double) -> (Double, Double) -> Globe
trackballGlobe g@(Globe r l) from to =
  let lift (x, y) = let d = x * x + y * y
                    in onGlobe g (if d >= 1 then normalize (x, y, 0) else (x, y, sqrt (1 - d)))
  in Globe (compose (rotationBetween (lift to) (lift from)) r) l

-- | The view zoomed by a factor (less than 1 zooms in), keeping the point
-- of the sphere under a screen point where it is.
zoomGlobeAbout :: Globe -> (Double, Double) -> Double -> Globe
zoomGlobeAbout g@(Globe r l) s f =
  let g' = Globe r (clampGlobeZoom (l * f))
  in case (screenToSphere g s, screenToSphere g' s) of
       (Just p, Just p') -> g' { globeRotation = compose (rotationBetween p' p) r }
       _ -> g'

-- | Interpolate views: the rotations along the sphere, the zooms
-- geometrically.
slerpGlobe :: Globe -> Globe -> Double -> Globe
slerpGlobe (Globe r1 l1) (Globe r2 l2) t = Globe (slerp r1 r2 t) (l1 ** (1 - t) * l2 ** t)

-- | A view as the Möbius map @(a, b, c, d)@ taking a point of the globe
-- in the viewer's frame, by its homogeneous stereographic coordinates
-- @[u : v]@ (@w = u / v@), to the point of the sphere it shows:
-- @[a u + b v : c u + d v]@. It's the rotation's Möbius map (found from
-- where it takes 0, ∞ and 1), after the dilation by λ.
globeMobius :: Globe -> (Complex Double, Complex Double, Complex Double, Complex Double)
globeMobius (Globe r l) =
  let hom (x, y, z) = if z >= 0 then (x :+ y, (1 + z) :+ 0) else ((1 - z) :+ 0, x :+ negate y)
      (p0a, p0b) = hom (rotateBy r (0, 0, 1))
      (pia, pib) = hom (rotateBy r (0, 0, -1))
      (p1a, p1b) = hom (rotateBy r (1, 0, 0))
      k = (p0b * p1a - p0a * p1b) / (pia * p1b - pib * p1a)
      (a, b, c, d) = (k * pia * (l :+ 0), p0a, k * pib * (l :+ 0), p0b)
      -- (scaled so that the determinant is 1, so the entries are of a size)
      s = sqrt (a * d - b * c)
  in (a / s, b / s, c / s, d / s)

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
visibleRuns proj = go [] . map (projectToScreen proj)
  where
    go run = \case
      [] -> finish run
      Just p : rest -> go (p : run) rest
      Nothing : rest -> finish run ++ go [] rest
    finish run = [ reverse run | length run >= 2 ]

-- | Points along a straight segment of the plane, ends included: on the
-- sphere, a segment is an arc, drawn as this many pieces.
sampleSegment :: Int -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
sampleSegment n (x1, y1) (x2, y2) =
  [ (x1 + t * (x2 - x1), y1 + t * (y2 - y1)) | k <- [0 .. n], let t = fromIntegral k / fromIntegral n ]

-- | Points along a geodesic, ends included, as this many pieces: in the
-- hyperbolic plane, the shortest path there (see
-- 'Actor.Viewer.Disk.geodesicPoints'); anywhere else, the straight
-- segment ('sampleSegment').
sampleGeodesic :: Projection -> Int -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
sampleGeodesic = \case
  HyperbolicProjection model _ _ -> geodesicPoints model
  _ -> sampleSegment

-- | Points around a circle of the plane, the first repeated at the end.
sampleCircle :: Int -> (Double, Double) -> Double -> [(Double, Double)]
sampleCircle n (cx, cy) r =
  [ (cx + r * cos a, cy + r * sin a) | k <- [0 .. n], let a = 2 * pi * fromIntegral k / fromIntegral n ]

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
sphereViewName = "[internal] sphere view"

-- | The names under which a sphere viewer's code is given its view's
-- Möbius map, @a@, @b@, @c@ and @d@ (see 'globeMobius').
globeMobiusNames :: [String]
globeMobiusNames = [ "[internal] sphere view " ++ [k] | k <- "abcd" ]

-- | The name under which a sphere viewer's code is given its zoom (see
-- 'Globe').
sphereZoomName :: String
sphereZoomName = "[internal] sphere zoom"

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

-- | What a complex viewer's code is given, besides its configuration, so
-- that it can be shown on the plane or on the sphere.
sphereViewEntries :: [(String, SomeType)]
sphereViewEntries = (sphereViewName, SomeType BooleanType)
                  : (sphereShadingName, SomeType RealType)
                  : (sphereZoomName, SomeType RealType)
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
-- >   z : Ĉ := [a u + b v : c u + d v]  -- the view's Möbius map
-- >   pixel_size <- dx λ (1 + |w|²) / (1 + λ² |w|²)    -- w = u / v
-- >   body
-- >   light := ((x, y, s) · L + 1) / 2
-- >   color <- blend(1 - shading (1 - light), color, black)
--
-- On the visible side @s >= 0@, so @[u : v]@ is computed without
-- cancellation, and the Möbius map (the view's rotation after its zoom:
-- see 'globeMobius') takes it to the exact point of the sphere, ∞
-- included, however far the view is zoomed in. @pixel_size@ (when the
-- viewer names it) is a pixel's size on the sphere there: the zoom
-- shrinks the middle of the globe by λ, less towards the rim. A @z@ the
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
    w2 = "[internal] sphere.w2"
    mobius k = var ("[internal] sphere view " ++ [k])
    (ma, mb, mc, md) = (mobius 'a', mobius 'b', mobius 'c', mobius 'd')
    lam = var sphereZoomName
    complex x = e (ECast x ComplexT)
    point a b | expanded = e (EPair (complex a) (complex b))
              | otherwise = e (EHomogeneous [a, b])
    pointT = if expanded then Pair ComplexT ComplexT else ExtComplexT
    here = e (ECast (point (bin OpAdd (bin OpMul ma (var u)) (bin OpMul mb (var v)))
                           (bin OpAdd (bin OpMul mc (var u)) (bin OpMul md (var v)))) pointT)
    -- (a pixel's size on the sphere: the screen's, times the zoom's
    -- stretch at w)
    stretch = bin OpDiv (bin OpMul lam (bin OpAdd (real 1) (var w2)))
                        (bin OpAdd (real 1) (bin OpMul (bin OpMul lam lam) (var w2)))
    lift =
      [ stmt (SDecl s (Just RealT) (e (EApp "sqrt" (bin OpSub (real 1) d2))))
      , stmt (SDecl u (Just ComplexT) (bin OpAdd sx (bin OpMul sy i)))
      , stmt (SDecl v (Just RealT) (bin OpAdd (real 1) (var s)))
      , stmt (SDecl w2 (Just RealT) (bin OpDiv d2 (bin OpMul (var v) (var v)))) ]
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
