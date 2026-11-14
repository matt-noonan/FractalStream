{- |
Module      : Actor.Viewer.Disk
Description : The geometry of a viewer that shows the hyperbolic plane, in the Poincaré disk

A disk viewer shows the hyperbolic plane as the Poincaré disk: the unit
disk, with the metric @2 |dz| / (1 - |z|²)@. Its screen is measured in
units of the disk's radius (as on the sphere), and the window can be
moved and zoomed on it like a plane viewer's (by 'vCenter' and the pixel
size), so the disk can be magnified anywhere.

What the screen shows is the disk moved by an /isometry/, the view: the
screen point @s@ shows the model point @V(s)@. The orientation-preserving
isometries of the disk are the Möbius maps

> V(s) = (a s + b) / (b̄ s + ā),   |a|² - |b|² = 1,

kept here as the pair @(a, b)@ (the matrix @[[a, b], [b̄, ā]]@ of SU(1, 1),
up to sign). Dragging moves the view by the hyperbolic translation that
keeps the model point under the pointer; that's how the whole plane can
be visited, while the screen's own (Euclidean) zoom magnifies details.

A viewer's code sees the model point as a plain complex number, through
the homogeneous pair @[a s + b : b̄ s + ā]@ (divided at the very end), so
that later the view itself can be moved by a group's elements before the
division (see HYPERBOLIC_PLAN.md: points near the circle keep few digits
as a plain complex number).
-}
module Actor.Viewer.Disk
  ( -- * Isometries
    Isometry(..)
  , identityIsometry
  , normalizeIsometry
  , composeIsometry
  , inverseIsometry
  , applyIsometry
  , translationTo
  , transvection
  , isometryFacing
  , isometryCenter
  , dragIsometry
  , interpolateIsometry
    -- * Points
  , screenToDisk
  , diskToScreen
    -- * Models and displays: the disk and the half-plane
  , HyperbolicModel(..)
  , HyperbolicDisplay(..)
  , Mobius(..)
  , applyMobius
  , composeMobius
  , normalizeMobius
  , identityMobius
  , diskToModel
  , displayToDisk
  , inverseMobius
  , isometryMobius
  , cayley
  , viewMobius
  , mobiusEntries
  , onDisplay
  , inModel
  , modelToDisk
  , displayToDiskPoint
  , displayMiddle
  , displayScreen
  , axisAtBottom
  , viewShowing
  , screenToModelH
  , modelToScreenH
  , dragOnDisplay
    -- * Defaults
  , defaultDiskPixelSize
  , defaultHalfPlanePixelSize
  , diskBackground
    -- * A viewer's code, in the hyperbolic plane
  , hyperbolicViewName
  , halfPlaneDisplayName
  , mobiusNames
  , hyperbolicViewEntries
  , inTheHyperbolicPlane
  ) where

import Data.Color (Color, rgbToColor)
import Data.Complex
import Language.Type
import Language.Parser.SourceRange (SourceRange(..))
import Language.Code.Syntax
import Language.Value.Syntax

-- | An orientation-preserving isometry of the disk, @s ↦ (a s + b) /
-- (b̄ s + ā)@ with @|a|² - |b|² = 1@.
data Isometry = Isometry !(Complex Double) !(Complex Double)
  deriving (Eq, Show)

identityIsometry :: Isometry
identityIsometry = Isometry 1 0

-- | Scale @(a, b)@ back to @|a|² - |b|² = 1@ (after rounding has moved it
-- a little); a degenerate pair (not an isometry) becomes the identity.
normalizeIsometry :: Isometry -> Isometry
normalizeIsometry (Isometry a b)
  | not (d > 0) || isInfinite d = identityIsometry
  | otherwise = Isometry (a / k) (b / k)
  where
    d = magnitude a ^ (2 :: Int) - magnitude b ^ (2 :: Int)
    k = sqrt d :+ 0

-- | @composeIsometry f g@ is @f . g@: first @g@, then @f@.
composeIsometry :: Isometry -> Isometry -> Isometry
composeIsometry (Isometry a1 b1) (Isometry a2 b2) =
  normalizeIsometry (Isometry (a1 * a2 + b1 * conjugate b2) (a1 * b2 + b1 * conjugate a2))

inverseIsometry :: Isometry -> Isometry
inverseIsometry (Isometry a b) = Isometry (conjugate a) (negate b)

applyIsometry :: Isometry -> Complex Double -> Complex Double
applyIsometry (Isometry a b) s = (a * s + b) / (conjugate b * s + conjugate a)

-- | The hyperbolic translation along the diameter through @p@ that takes
-- 0 to @p@ (a point of the disk).
translationTo :: Complex Double -> Isometry
translationTo p = Isometry (k :+ 0) (p * (k :+ 0))
  where k = 1 / sqrt (1 - magnitude p ^ (2 :: Int))

-- | The hyperbolic translation along the geodesic through @p@ and @q@
-- that takes @p@ to @q@ (no extra turn): the one through 0, moved to @p@.
transvection :: Complex Double -> Complex Double -> Isometry
transvection p q =
  let toP = translationTo p
      q' = applyIsometry (inverseIsometry toP) q
  in composeIsometry toP (composeIsometry (translationTo q') (inverseIsometry toP))

-- | The view that shows the model point @c@ at the screen's center (and
-- the directions at 0 the same way round as on the plane).
isometryFacing :: (Double, Double) -> Isometry
isometryFacing (x, y) = translationTo (x :+ y)

-- | The model point a view shows at the screen's center.
isometryCenter :: Isometry -> (Double, Double)
isometryCenter v = let x :+ y = applyIsometry v 0 in (x, y)

-- | The view after dragging from one screen point to another (in units of
-- the disk's radius), both on the disk: the model point that was under
-- the first is under the second, moved by a hyperbolic translation. A
-- point off the disk is pulled just inside it.
dragIsometry :: Isometry -> (Double, Double) -> (Double, Double) -> Isometry
dragIsometry v from to = composeIsometry v (transvection (inside to) (inside from))
  where
    inside (x, y) =
      let r = sqrt (x * x + y * y)
      in if r < 0.999 then x :+ y else (x :+ y) * ((0.999 / r) :+ 0)

-- | A path of views, from @v1@ (at 0) to @v2@ (at 1): the center moves
-- along the geodesic between their centers (at constant hyperbolic
-- speed), and the turn about it changes evenly, the shorter way round.
interpolateIsometry :: Isometry -> Isometry -> Double -> Isometry
interpolateIsometry v1 v2 t =
  let c1 = applyIsometry v1 0
      c2 = applyIsometry v2 0
      -- the turn of a view about its center: V = translationTo c . turn
      turnOf v cv = let Isometry a _ = composeIsometry (inverseIsometry (translationTo cv)) v in 2 * phase a
      th1 = turnOf v1 c1
      th2 = turnOf v2 c2
      dth = let d = th2 - th1 in d - 2 * pi * fromIntegral (round (d / (2 * pi)) :: Int)
      th = th1 + t * dth
      -- the center, along the geodesic: c2 seen from c1 is q, in the
      -- direction u at hyperbolic distance 2 atanh |q|
      toC1 = translationTo c1
      q = applyIsometry (inverseIsometry toC1) c2
      r = magnitude q
      c | r < 1e-15 = c1
        | otherwise = applyIsometry toC1 (q * ((tanh (t * atanh r) / r) :+ 0))
  in composeIsometry (translationTo c) (Isometry (cis (th / 2)) 0)

-- | The model point under a screen point, if it's on the disk.
screenToDisk :: Isometry -> (Double, Double) -> Maybe (Double, Double)
screenToDisk v (x, y)
  | x * x + y * y >= 1 = Nothing
  | otherwise = let u :+ w = applyIsometry v (x :+ y) in Just (u, w)

-- | Where a model point is on the screen, if it's in the disk.
diskToScreen :: Isometry -> (Double, Double) -> Maybe (Double, Double)
diskToScreen v (x, y)
  | not (x * x + y * y < 1) = Nothing
  | otherwise = let u :+ w = applyIsometry (inverseIsometry v) (x :+ y) in Just (u, w)

------------------------------------------------------------------------
-- Models and displays

-- | Where a script's points are: the unit disk, or the upper half-plane
-- (set by its @view=@: @disk@ or @halfplane@).
data HyperbolicModel = DiskModel | HalfPlaneModel
  deriving (Eq, Show)

-- | How the hyperbolic plane is drawn on the screen: as the disk (screen
-- points in the unit disk) or as the half-plane (screen points with
-- positive imaginary part). A viewer can switch between them; its view
-- (an 'Isometry' of the disk) is the same either way.
data HyperbolicDisplay = DiskDisplay | HalfPlaneDisplay
  deriving (Eq, Show)

-- | A Möbius map @s ↦ (a s + b) / (c s + d)@, kept with @ad - bc = 1@.
data Mobius = Mobius !(Complex Double) !(Complex Double) !(Complex Double) !(Complex Double)
  deriving (Eq, Show)

applyMobius :: Mobius -> Complex Double -> Complex Double
applyMobius (Mobius a b c d) s = (a * s + b) / (c * s + d)

normalizeMobius :: Mobius -> Mobius
normalizeMobius (Mobius a b c d) = let k = sqrt (a * d - b * c) in Mobius (a / k) (b / k) (c / k) (d / k)

-- | @composeMobius f g@ is @f . g@.
composeMobius :: Mobius -> Mobius -> Mobius
composeMobius (Mobius a b c d) (Mobius e f g h) =
  normalizeMobius (Mobius (a * e + b * g) (a * f + b * h) (c * e + d * g) (c * f + d * h))

inverseMobius :: Mobius -> Mobius
inverseMobius (Mobius a b c d) = Mobius d (negate b) (negate c) a

isometryMobius :: Isometry -> Mobius
isometryMobius (Isometry a b) = Mobius a b (conjugate b) (conjugate a)

identityMobius :: Mobius
identityMobius = Mobius 1 0 0 1

-- | The Cayley map, taking the upper half-plane to the disk: @s ↦ (s - i)
-- / (s + i)@ (so @i@ goes to 0, and the real axis to the circle).
cayley :: Mobius
cayley = normalizeMobius (Mobius 1 (0 :+ (-1)) 1 (0 :+ 1))

-- | From a display's screen points to the disk.
displayToDisk :: HyperbolicDisplay -> Mobius
displayToDisk = \case
  DiskDisplay -> identityMobius
  HalfPlaneDisplay -> cayley

-- | From the disk to a model's points.
diskToModel :: HyperbolicModel -> Mobius
diskToModel = \case
  DiskModel -> identityMobius
  HalfPlaneModel -> inverseMobius cayley

-- | What a viewer's code computes its point from: the screen point (of a
-- display), moved by the view (in the disk), as a point of the model.
viewMobius :: HyperbolicModel -> HyperbolicDisplay -> Isometry -> Mobius
viewMobius model display v =
  composeMobius (diskToModel model) (composeMobius (isometryMobius v) (displayToDisk display))

-- | A Möbius map's entries, in the order of 'mobiusNames'.
mobiusEntries :: Mobius -> [Double]
mobiusEntries (Mobius (ar :+ ai) (br :+ bi) (cr :+ ci) (dr :+ di)) = [ar, ai, br, bi, cr, ci, dr, di]

-- | Whether a screen point shows the hyperbolic plane, on a display.
onDisplay :: HyperbolicDisplay -> (Double, Double) -> Bool
onDisplay display (x, y) = case display of
  DiskDisplay -> x * x + y * y < 1
  HalfPlaneDisplay -> y > 0

-- | Whether a point is in a model.
inModel :: HyperbolicModel -> (Double, Double) -> Bool
inModel model (x, y) = case model of
  DiskModel -> x * x + y * y < 1
  HalfPlaneModel -> y > 0

-- | A model's point, in the disk (where views act).
modelToDisk :: HyperbolicModel -> (Double, Double) -> (Double, Double)
modelToDisk model (x, y) = let u :+ w = applyMobius (inverseMobius (diskToModel model)) (x :+ y) in (u, w)

-- | A display's screen point, in the disk.
displayToDiskPoint :: HyperbolicDisplay -> (Double, Double) -> (Double, Double)
displayToDiskPoint display (x, y) = let u :+ w = applyMobius (displayToDisk display) (x :+ y) in (u, w)

-- | The screen point showing the disk's center (where a view's center
-- is): the disk's center, or @i@ on the half-plane.
displayMiddle :: HyperbolicDisplay -> (Double, Double)
displayMiddle = \case
  DiskDisplay -> (0, 0)
  HalfPlaneDisplay -> (0, 1)

-- | A display's screen in a window @h@ pixels high, at a zoom (a pixel
-- size): the screen point in the window's middle, and the pixel size. The
-- disk is centered in the window. The half-plane has its real axis along
-- the window's bottom edge (no space is wasted below it), and the view's
-- center ('displayMiddle', @i@) 1/zoom pixels above that, or in the
-- window's middle if that's lower.
displayScreen :: HyperbolicDisplay -> Int -> Double -> ((Double, Double), Double)
displayScreen DiskDisplay _ q = ((0, 0), q)
displayScreen HalfPlaneDisplay h q =
  let px = max q (2 / fromIntegral (max 1 h))
  in ((0, fromIntegral h * px / 2), px)

-- | The screen point in the middle of a half-plane display's window @h@
-- pixels high, at a pixel size, keeping its real axis along the window's
-- bottom edge (after the window's resized, or zoomed, say).
axisAtBottom :: Int -> Double -> (Double, Double) -> (Double, Double)
axisAtBottom h px (cx, _) = (cx, fromIntegral h * px / 2)

-- | The view showing a model point at a screen point (the window's
-- middle, say), by a hyperbolic translation from the view facing it.
viewShowing :: HyperbolicModel -> HyperbolicDisplay -> (Double, Double) -> (Double, Double) -> Isometry
viewShowing model display c s =
  dragOnDisplay display (isometryFacing (modelToDisk model c)) (displayMiddle display) s

-- | The model point under a screen point, if it shows the hyperbolic plane.
screenToModelH :: HyperbolicModel -> HyperbolicDisplay -> Isometry -> (Double, Double) -> Maybe (Double, Double)
screenToModelH model display v s@(x, y)
  | not (onDisplay display s) = Nothing
  | otherwise = let u :+ w = applyMobius (viewMobius model display v) (x :+ y) in Just (u, w)

-- | Where a model point is on the screen, if it's shown.
modelToScreenH :: HyperbolicModel -> HyperbolicDisplay -> Isometry -> (Double, Double) -> Maybe (Double, Double)
modelToScreenH model display v p@(x, y)
  | not (inModel model p) = Nothing
  | otherwise =
      let u :+ w = applyMobius (inverseMobius (viewMobius model display v)) (x :+ y)
      in if onDisplay display (u, w) then Just (u, w) else Nothing

-- | The view after dragging from one screen point to another, on a
-- display: the model point under the first is under the second (see
-- 'dragIsometry', in the disk).
dragOnDisplay :: HyperbolicDisplay -> Isometry -> (Double, Double) -> (Double, Double) -> Isometry
dragOnDisplay display v from to =
  dragIsometry v (displayToDiskPoint display (pull from)) (displayToDiskPoint display (pull to))
  where
    -- (a point below the half-plane counts as just above the real axis)
    pull (x, y) = case display of
      HalfPlaneDisplay | y <= 0 -> (x, 1e-6)
      _ -> (x, y)

-- | The size of a pixel (in units of the disk's radius) that a disk viewer
-- starts with, unless its script says: the disk fills most of a
-- 600-pixel window.
defaultDiskPixelSize :: Double
defaultDiskPixelSize = 1 / 280

-- | The size of a pixel a half-plane display starts with, unless its
-- script says: the window, centered on @i@, shows the real axis a quarter
-- of the way up a 600-pixel window.
defaultHalfPlanePixelSize :: Double
defaultHalfPlanePixelSize = 1 / 150

-- | The color off the disk.
diskBackground :: Color
diskBackground = rgbToColor (48, 48, 48)

------------------------------------------------------------------------
-- A viewer's code, in the disk

-- | The name under which a hyperbolic viewer's code is told that it
-- shows the hyperbolic plane (a truth value no script can name; always
-- true, for now).
hyperbolicViewName :: String
hyperbolicViewName = "[internal] disk view"

-- | The name under which it's told whether its display is the half-plane
-- (else the disk).
halfPlaneDisplayName :: String
halfPlaneDisplayName = "[internal] half-plane display"

-- | The names under which it's given its 'viewMobius': the real and
-- imaginary parts of @a@, @b@, @c@, @d@.
mobiusNames :: [String]
mobiusNames = [ "[internal] hyperbolic " ++ n
              | n <- ["a.re", "a.im", "b.re", "b.im", "c.re", "c.im", "d.re", "d.im"] ]

-- | What a hyperbolic viewer's code is given, besides its configuration.
hyperbolicViewEntries :: [(String, SomeType)]
hyperbolicViewEntries = (hyperbolicViewName, SomeType BooleanType)
                      : (halfPlaneDisplayName, SomeType BooleanType)
                      : [ (n, SomeType RealType) | n <- mobiusNames ]

-- | A hyperbolic viewer's code (with coordinate @z@, and pixel size @px@),
-- wrapped so that @z@ is the model point under the pixel, and a pixel off
-- the display (the disk, or the half-plane) gets the background color:
--
-- > if (half-plane and y <= 0) or (not half-plane and x² + y² >= 1):
-- >   color <- background
-- > else:
-- >   s := x + y i
-- >   num := a s + b          -- the homogeneous [a s + b : c s + d]
-- >   den := c s + d
-- >   z <- num / den
-- >   px <- px / |den|²
-- >   body
--
-- with @(a, b, c, d)@ the 'viewMobius' (determinant 1). The pixel size is,
-- as everywhere, the model width of a pixel: here it depends on the
-- pixel, since the view stretches the plane (by |dz/ds| = 1 / |den|²). In
-- the disk model, a pixel's hyperbolic size is @2 px / (1 - |z|²)@; in the
-- half-plane model, @px / im(z)@.
--
-- A viewer with a group (see "Actor.Viewer.Group") sets @z@ and @px@ its
-- own way, from the names of @num@ and @den@ (the last two lines above).
inTheHyperbolicPlane :: String -> String -> Maybe (String -> String -> [Stmt]) -> Stmt -> Stmt
inTheHyperbolicPlane z px mfold body =
  stmt (SIf offDisplay (stmt (SSet "color" background))
                       (stmt (SBlock (lift ++ [body]))))
  where
    stmt = Stmt NoSourceRange
    e = Syn NoSourceRange
    var = e . EVar
    real d = e (ELit (show d) (SomeHaskellType RealType d))
    bin op p q = e (EBin op p q)
    sx = var "[internal] x"
    sy = var "[internal] y"
    i = e (ELit "i" (SomeHaskellType ComplexType (0 :+ 1)))
    cplx re im = bin OpAdd re (bin OpMul im i)
    halfPlane = var halfPlaneDisplayName
    d2 = bin OpAdd (bin OpMul sx sx) (bin OpMul sy sy)
    offDisplay = bin OpOr (bin OpAnd halfPlane (bin OpLe sy (real 0)))
                          (bin OpAnd (e (EUn OpNot halfPlane)) (bin OpGe d2 (real 1)))
    background = e (ELit "the background" (SomeHaskellType ColorType diskBackground))
    (ma, mb, mc, md) = case map var mobiusNames of
      [ar, ai, br, bi, cr, ci, dr, di] -> (cplx ar ai, cplx br bi, cplx cr ci, cplx dr di)
      _ -> error "internal error: a Möbius map has eight entries"
    s = "[internal] disk.s"
    num = "[internal] disk.num"
    den = "[internal] disk.den"
    lift =
      [ stmt (SDecl s (Just ComplexT) (cplx sx sy))
      , stmt (SDecl num (Just ComplexT) (bin OpAdd (bin OpMul ma (var s)) mb))
      , stmt (SDecl den (Just ComplexT) (bin OpAdd (bin OpMul mc (var s)) md)) ]
      ++ case mfold of
           Just fold -> fold num den
           Nothing ->
             [ stmt (SSet z (bin OpDiv (var num) (var den)))
             , stmt (SSet px (bin OpDiv (var px) (bin OpMul (e (EUn OpAbs (var den))) (e (EUn OpAbs (var den)))))) ]
