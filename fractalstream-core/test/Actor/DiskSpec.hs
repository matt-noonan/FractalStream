-- | Disk viewers: the geometry of the Poincaré disk's views.
module Actor.DiskSpec (spec) where

import Test.Hspec

import Actor.Script
import Actor.Event (ViewKind(..))
import Actor.Ensemble (checkScript)
import Actor.Viewer.Disk

import Data.Complex
import Data.List (isInfixOf)

close :: Double -> Double -> Bool
close a b = abs (a - b) < 1e-9

close2 :: (Double, Double) -> (Double, Double) -> Bool
close2 (a, b) (c, d) = close a c && close b d

closeC :: Complex Double -> Complex Double -> Bool
closeC z w = magnitude (z - w) < 1e-9

-- | The hyperbolic distance between two points of the disk.
dist :: Complex Double -> Complex Double -> Double
dist z w = 2 * atanh (magnitude ((z - w) / (1 - conjugate w * z)))

-- | Some views to try things with.
views :: [Isometry]
views =
  [ identityIsometry
  , isometryFacing (0.5, 0.2)
  , isometryFacing (-0.9, 0.3)
  , composeIsometry (isometryFacing (0.99, 0)) (Isometry (cis 0.7) 0)
  , composeIsometry (isometryFacing (0.2, -0.6)) (isometryFacing (-0.4, 0.5)) ]

points :: [Complex Double]
points = [ 0, 0.5, 0 :+ 0.5, (-0.3) :+ 0.7, 0.9 :+ (-0.1), 0.001 :+ 0.002 ]

spec :: Spec
spec = do
 describe "a disk viewer's header" $ do

  it "reads `view=disk`" $
    case parseScript "@viewer \"V\" var=z view=disk\npass\n" of
      Right [ViewerStanza{..}] -> vsView `shouldBe` DiskView
      _ -> expectationFailure "expected one viewer"

  it "rejects real viewers in the disk" $
    case parseScript "@viewer \"V\" xvar=x yvar=y view=disk\npass\n" of
      Left (_, err) -> err `shouldSatisfy` isInfixOf "Only a complex viewer"
      Right _ -> expectationFailure "expected an error"

  it "reads `view=halfplane`, and checks that `center` is above the real axis" $ do
    case parseScript "@viewer \"V\" var=z view=halfplane\npass\n" of
      Right [ViewerStanza{..}] -> vsView `shouldBe` HalfPlaneView
      _ -> expectationFailure "expected one viewer"
    checkScript "@viewer \"V\" var=z view=halfplane\ncolor <- if im(z) > 1 then red else blue\n" `shouldBe` Right ()

  it "builds a disk viewer, whose pixel size is the local model width" $ do
    checkScript "@viewer \"V\" var=z view=disk center=\"0.5 + 0.25i\"\ncolor <- if |z| < 0.5 and pixel_size > 0 then red else blue\n"
      `shouldBe` Right ()
    checkScript "@config \"P\"\nC : C := 0\n\n@viewer \"V\" var=C view=disk\nz := 0\niterate z -> z^2 + C until z escapes\n"
      `shouldBe` Right ()

 describe "the half-plane, and the disk, as models and displays" $ do

  let models = [DiskModel, HalfPlaneModel]
      displays = [DiskDisplay, HalfPlaneDisplay]
      shown DiskDisplay = [(0, 0), (0.5, 0.5), (-0.9, 0.1)]
      shown HalfPlaneDisplay = [(0, 1), (0.5, 0.2), (-3, 7)]

  it "takes the half-plane to the disk by the Cayley map: i to 0" $ do
    applyMobius cayley (0 :+ 1) `shouldSatisfy` closeC 0
    sequence_ [ magnitude (applyMobius cayley z) `shouldSatisfy` (< 1) | z <- [0 :+ 0.01, 5 :+ 2, (-3) :+ 100] ]

  it "takes screen points to each model and back, on each display" $
    sequence_ [ (screenToModelH m d v s >>= modelToScreenH m d v) `shouldSatisfy` maybe False (close2 s)
              | m <- models, d <- displays, v <- views, s <- shown d ]

  it "shows the half-plane model on the half-plane display as it is, facing i" $
    sequence_ [ screenToModelH HalfPlaneModel HalfPlaneDisplay identityIsometry s `shouldSatisfy` maybe False (close2 s)
              | s <- shown HalfPlaneDisplay ]

  it "shows the view's center in the middle of each display" $
    sequence_ [ screenToModelH m d (isometryFacing (modelToDisk m c)) (displayMiddle d) `shouldSatisfy` maybe False (close2 c)
              | m <- models, d <- displays, let c = if m == DiskModel then (0.3, -0.2) else (1, 2) ]

  it "lays the half-plane out with its real axis along the window's bottom edge" $ do
    -- (a window 600 pixels high: the middle row's 300 pixels up)
    displayScreen HalfPlaneDisplay 600 (1 / 150) `shouldBe` ((0, 2), 1 / 150)
    -- (i no higher than the middle)
    displayScreen HalfPlaneDisplay 600 (1 / 1000) `shouldBe` ((0, 1), 1 / 300)
    displayScreen DiskDisplay 600 (1 / 280) `shouldBe` ((0, 0), 1 / 280)
    axisAtBottom 400 (1 / 100) (0.5, 7) `shouldBe` (0.5, 2)

  it "shows a model point at a screen point" $
    sequence_ [ screenToModelH m d (viewShowing m d c s) s `shouldSatisfy` maybe False (close2 c)
              | m <- models, d <- displays, s <- shown d
              , c <- if m == DiskModel then [(0.3, -0.2), (0, 0.9)] else [(1, 2), (-4, 0.01)] ]

  it "keeps the point under the pointer there while dragging, on the half-plane" $
    sequence_ [ case screenToModelH m HalfPlaneDisplay v from of
                  Nothing -> expectationFailure "off the half-plane"
                  Just p -> modelToScreenH m HalfPlaneDisplay (dragOnDisplay HalfPlaneDisplay v from to) p
                              `shouldSatisfy` maybe False (\s -> abs (fst s - fst to) < 1e-6 && abs (snd s - snd to) < 1e-6)
              | m <- models, v <- views, (from, to) <- [((0, 1), (0.5, 2)), ((-1, 0.5), (2, 3))] ]

 describe "the disk's geometry" $ do

  it "moves points by isometries: distances are kept" $
    sequence_ [ dist (applyIsometry v z) (applyIsometry v w) `shouldSatisfy` close (dist z w)
              | v <- views, z <- points, w <- points ]

  it "keeps the disk: |a|² - |b|² = 1, and points stay inside" $
    sequence_ $ [ let Isometry a b = v in (magnitude a ^ (2 :: Int) - magnitude b ^ (2 :: Int)) `shouldSatisfy` close 1
                | v <- views ]
             ++ [ magnitude (applyIsometry v z) `shouldSatisfy` (< 1) | v <- views, z <- points ]

  it "composes, and undoes, views" $
    sequence_ $ [ applyIsometry (composeIsometry v w) z `shouldSatisfy` closeC (applyIsometry v (applyIsometry w z))
                | v <- views, w <- views, z <- points ]
             ++ [ applyIsometry (inverseIsometry v) (applyIsometry v z) `shouldSatisfy` closeC z
                | v <- views, z <- points ]

  it "faces a point: it's at the screen's center" $
    sequence_ [ isometryCenter (isometryFacing c) `shouldSatisfy` close2 c
              | c <- [(0, 0), (0.5, 0.2), (-0.9, 0.3), (0, -0.999)] ]

  it "translates along the geodesic between two points, without turning" $ do
    let p = 0.3 :+ 0.4
        q = (-0.5) :+ 0.1
        t = transvection p q
    applyIsometry t p `shouldSatisfy` closeC q
    -- a translation (no turn) fixes the two ends of its geodesic, on the
    -- circle: the ends of the diameter through q' = p⁻¹(q), moved to p
    let toP = isometryFacing (realPart p, imagPart p)
        q' = applyIsometry (inverseIsometry toP) q
        u = q' / (magnitude q' :+ 0)
        ends = [ applyIsometry toP u, applyIsometry toP (negate u) ]
    sequence_ [ applyIsometry t end `shouldSatisfy` closeC end | end <- ends ]
    -- with p = 0, it's the plain translation along the diameter
    applyIsometry (transvection 0 q) 0 `shouldSatisfy` closeC q
    let Isometry a _ = transvection 0 q in phase a `shouldSatisfy` close 0

  it "takes screen points to the disk and back, and shows nothing off it" $ do
    sequence_ [ (screenToDisk v s >>= diskToScreen v) `shouldSatisfy` maybe False (close2 s)
              | v <- views, s <- [(0, 0), (0.5, 0.5), (-0.9, 0.1), (0, -0.99)] ]
    screenToDisk identityIsometry (0.8, 0.8) `shouldBe` Nothing
    diskToScreen identityIsometry (1, 0) `shouldBe` Nothing

  it "keeps the point under the pointer there while dragging" $
    sequence_ [ case screenToDisk v from of
                  Nothing -> expectationFailure "off the disk"
                  Just p -> diskToScreen (dragIsometry v from to) p `shouldSatisfy` maybe False (close2 to)
              | v <- views, (from, to) <- [((0, 0), (0.3, 0.1)), ((0.2, -0.4), (-0.5, 0.3)), ((0.9, 0), (0.1, 0))] ]

  it "interpolates between views, ending at each, at an even pace" $ do
    let v1 = isometryFacing (0.5, 0.2)
        v2 = composeIsometry (isometryFacing (-0.7, -0.1)) (Isometry (cis 2) 0)
        at t = interpolateIsometry v1 v2 t
        c t = let (x, y) = isometryCenter (at t) in x :+ y
    sequence_ [ applyIsometry (at 0) z `shouldSatisfy` closeC (applyIsometry v1 z) | z <- points ]
    sequence_ [ applyIsometry (at 1) z `shouldSatisfy` closeC (applyIsometry v2 z) | z <- points ]
    dist (c 0) (c 0.5) `shouldSatisfy` close (dist (c 0.5) (c 1))
