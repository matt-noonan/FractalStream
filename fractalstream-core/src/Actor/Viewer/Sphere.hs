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
  , sampleCircle
    -- * Defaults
  , defaultSpherePixelSize
  , sphereBackground
    -- * A viewer's code, on the sphere
  , sphereViewName
  , rotationNames
  , sphereShadingName
  , sphereShading
  , sphereLight
  , sphereViewEntries
  , onTheSphere
  ) where

import Actor.Viewer.Disk (Isometry, HyperbolicModel, HyperbolicDisplay, modelToScreenH)
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
rotationFacing :: (Double, Double) -> Rotation
rotationFacing z = rotationBetween (0, 0, 1) (planeToSphere z)

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

-- | The point of the sphere (in the sphere's frame) under a screen
-- point, if it's on the globe.
screenToSphere :: Rotation -> (Double, Double) -> Maybe Vec3
screenToSphere r (x, y)
  | d > 1 = Nothing
  | otherwise = Just (rotateBy r (x, y, sqrt (1 - d)))
  where d = x * x + y * y

-- | Where a point of the sphere is on the screen, if it's on the
-- visible side.
sphereToScreen :: Rotation -> Vec3 -> Maybe (Double, Double)
sphereToScreen r p =
  let (x, y, z) = unrotateBy r p
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
screenToPlane :: Rotation -> (Double, Double) -> Maybe (Double, Double)
screenToPlane r = fmap sphereToPlane . screenToSphere r

-- | Where a complex number is on the screen, if it's on the visible side.
planeToScreen :: Rotation -> (Double, Double) -> Maybe (Double, Double)
planeToScreen r = sphereToScreen r . planeToSphere

------------------------------------------------------------------------
-- How model points reach the screen

-- | How a viewer's model points are placed on its screen plane: as they
-- are (the plane), on the globe, with its orientation, or in the
-- hyperbolic plane, from its model, moved by its view, onto its display
-- (see "Actor.Viewer.Disk").
data Projection = FlatProjection | GlobeProjection Rotation
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
  GlobeProjection r -> planeToScreen r
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

-- | The names under which a complex viewer's code is given its
-- rotation's matrix, by rows.
rotationNames :: [String]
rotationNames = [ "[internal] sphere rotation " ++ show i ++ show j | i <- [1 .. 3 :: Int], j <- [1 .. 3 :: Int] ]

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
                  : [ (n, SomeType RealType) | n <- rotationNames ]

-- | A complex viewer's code (with coordinate @z@), wrapped so that while
-- the viewer shows the sphere, @z@ is the point of the globe under the
-- pixel, and a pixel off the globe gets the background color:
--
-- > if sphere and x² + y² > 1:
-- >   color <- background
-- > else:
-- >   if sphere:
-- >     s := sqrt(1 - x² - y²)
-- >     (X, Y, Z) := rotation × (x, y, s)
-- >     z <- (X + Y i) / (1 + Z)      -- the homogeneous [X + iY : 1 + Z]
-- >   body
-- >   if sphere:
-- >     light := ((x, y, sqrt(1 - x² - y²)) · L + 1) / 2
-- >     color <- blend(1 - shading (1 - light), color, black)
--
-- On the plane, it's just the body (with @z@ already @x + y i@). The
-- shading darkens the globe away from the light ('sphereLight', @L@),
-- by the viewer's @[internal] sphere shading@ ('sphereShading'): a
-- "half Lambert" light, which falls off gently and never goes all the
-- way to black on the visible side.
onTheSphere :: String -> Stmt -> Stmt
onTheSphere z body =
  stmt (SIf offGlobe (stmt (SSet "color" background))
                     (stmt (SBlock [ stmt (SIf sphereOn (stmt (SBlock lift)) (stmt SPass))
                                   , body
                                   , stmt (SIf sphereOn (stmt (SBlock shade)) (stmt SPass)) ])))
  where
    stmt = Stmt NoSourceRange
    e = Syn NoSourceRange
    var = e . EVar
    real d = e (ELit (show d) (SomeHaskellType RealType d))
    bin op a b = e (EBin op a b)
    sx = var "[internal] x"
    sy = var "[internal] y"
    sphereOn = var sphereViewName
    d2 = bin OpAdd (bin OpMul sx sx) (bin OpMul sy sy)
    offGlobe = bin OpAnd sphereOn (bin OpGt d2 (real 1))
    background = e (ELit "the background" (SomeHaskellType ColorType sphereBackground))
    i = e (ELit "i" (SomeHaskellType ComplexType (0 :+ 1)))
    s = "[internal] sphere.s"
    cx = "[internal] sphere.X"
    cy = "[internal] sphere.Y"
    cz = "[internal] sphere.Z"
    rows = chunk rotationNames
    chunk (a : b : c : rest) = [a, b, c] : chunk rest
    chunk _ = []
    row ns = foldr1 (bin OpAdd) (zipWith (\m v -> bin OpMul (var m) v) ns [sx, sy, var s])
    lift =
      [ stmt (SDecl s (Just RealT) (e (EApp "sqrt" (bin OpSub (real 1) d2)))) ]
      ++ [ stmt (SDecl c (Just RealT) (row ns)) | (c, ns) <- zip [cx, cy, cz] rows ]
      ++ [ stmt (SSet z (bin OpDiv (bin OpAdd (var cx) (bin OpMul (var cy) i))
                                   (bin OpAdd (real 1) (var cz)))) ]
    (lx, ly, lz) = sphereLight
    light = "[internal] sphere.light"
    shading = var sphereShadingName
    facing = foldr1 (bin OpAdd)
      [ bin OpMul (real lx) sx, bin OpMul (real ly) sy
      , bin OpMul (real lz) (e (EApp "sqrt" (bin OpSub (real 1) d2))) ]
    shade =
      [ stmt (SDecl light (Just RealT) (bin OpDiv (bin OpAdd facing (real 1)) (real 2)))
      , stmt (SSet "color" (e (EColor3 OpBlend
                                 (bin OpSub (real 1) (bin OpMul shading (bin OpSub (real 1) (var light))))
                                 (var "color")
                                 (e (ELit "black" (SomeHaskellType ColorType black)))))) ]
