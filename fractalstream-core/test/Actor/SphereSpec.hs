-- | Sphere viewers: the geometry of the globe, and @view=sphere@.
module Actor.SphereSpec (spec) where

import Test.Hspec

import Actor.Script
import Actor.Event (ViewKind(..))
import Actor.Ensemble (checkScript)
import Actor.Viewer.Sphere
import Actor.Viewer.ViewModel
import Actor.Viewer.Disk (HyperbolicModel(..), HyperbolicDisplay(..), identityIsometry)

import Data.List (isInfixOf)
import Data.Complex (Complex(..))

close :: Double -> Double -> Bool
close a b = abs (a - b) < 1e-9

close2 :: (Double, Double) -> (Double, Double) -> Bool
close2 (a, b) (c, d) = close a c && close b d

close3 :: Vec3 -> Vec3 -> Bool
close3 (a, b, c) (d, e, f) = close a d && close b e && close c f

-- | Some rotations to try things with.
rotations :: [Rotation]
rotations =
  [ noRotation
  , rotationFacing (1, 1)
  , rotationFacing (-0.3, 2)
  , rotationFacing (1e6, 0)
  , compose (rotationFacing (0.5, -0.2)) (rotationFacing (0, 3)) ]

-- | Some views: the rotations, unzoomed, zoomed in, and zoomed out.
globes :: [Globe]
globes = [ rotationGlobe r `composeGlobe` zoomed | r <- rotations
          , zoomed <- [ noGlobe, globeFacingRolled (0, 0) 0 0.001, globeFacingRolled (0, 0) 0 7
                      -- (zoomed about a point that isn't in the middle)
                      , zoomGlobeAbout noGlobe (0.3, 0.4) 0.01 ] ]

points :: [(Double, Double)]
points = [ (0, 0), (1, 0), (0, 1), (0.5, -0.25), (-2, 3), (0.001, 0.002), (40, -7) ]

spec :: Spec
spec = do

  describe "the globe's geometry" $ do

    it "takes complex numbers to the sphere and back" $
      mapM_ (\z -> sphereToPlane (planeToSphere z) `shouldSatisfy` close2 z) points

    it "puts 0 at the north pole and the unit circle on the equator" $ do
      planeToSphere (0, 0) `shouldSatisfy` close3 (0, 0, 1)
      planeToSphere (1, 0) `shouldSatisfy` close3 (1, 0, 0)
      planeToSphere (0, 1) `shouldSatisfy` close3 (0, 1, 0)
      planeToSphere (1 / 0, 0) `shouldSatisfy` close3 (0, 0, -1)

    it "takes screen points to the globe and back" $
      sequence_ [ fmap (sphereToScreen g) (screenToSphere g s) `shouldSatisfy` maybe False (maybe False (close2' s))
                | g <- globes, s <- [(0, 0), (0.5, 0.5), (-0.9, 0.1), (0, -0.95)] ]

    it "shows nothing off the globe, or on its far side" $ do
      screenToSphere noGlobe (0.8, 0.8) `shouldBe` Nothing
      planeToScreen noGlobe (1e6, 0) `shouldBe` Nothing

    it "faces 0 with no rotation, the real axis to the right and the imaginary axis up" $ do
      screenToPlane noGlobe (0, 0) `shouldSatisfy` maybe False (close2 (0, 0))
      planeToScreen noGlobe (0.1, 0) `shouldSatisfy` maybe False (\(x, y) -> x > 0 && close y 0)
      planeToScreen noGlobe (0, 0.1) `shouldSatisfy` maybe False (\(x, y) -> close x 0 && y > 0)

    it "turns the globe to face a point" $
      mapM_ (\z -> screenToPlane (globeFacing z) (0, 0) `shouldSatisfy` maybe False (close2 z))
            [ p | p <- points, p /= (0, 0) ]

    it "faces points near ∞ exactly" $
      mapM_ (\z@(a, b) -> globeCenter (globeFacing z) `shouldSatisfy` (\(c, d) -> abs (c / a - 1) < 1e-12 && abs (d - b) <= 1e-12 * abs a))
            [ (1e30, 0), (-3e20, 1e20), (7e15, -2e15) ]

    it "zooms about the middle: a zoom of λ shrinks it by λ" $ do
      let g = globeFacingRolled (2, 1) 0 0.001
      -- (a screen point near the middle shows a point λ times as far,
      -- chordally, as it would unzoomed)
      globeZoom g `shouldSatisfy` (\z -> abs (z / 0.001 - 1) < 1e-9)
      case (screenToSphere g (0.01, 0), screenToSphere (globeFacing (2, 1)) (0.01, 0)) of
        (Just p, Just q) -> let mid = planeToSphere (2, 1)
                                dist (a, b, c) (d, e, f) = sqrt ((a - d)^(2 :: Int) + (b - e)^(2 :: Int) + (c - f)^(2 :: Int))
                            in (dist p mid / dist q mid) `shouldSatisfy` (\r -> abs (r - 0.001) < 1e-6)
        _ -> expectationFailure "off the globe"

    it "keeps the point under the pointer there while zooming" $
      sequence_ [ case screenToSphere g s of
                    Nothing -> expectationFailure "off the globe"
                    Just p -> sphereToScreen (zoomGlobeAbout g s f) p `shouldSatisfy` maybe False (close2' s)
                | g <- globes, s <- [(0.3, 0.2), (-0.1, 0.5)], f <- [0.5, 2] ]

    it "turns what it shows rigidly when it's dragged, however it's zoomed" $
      sequence_ [ let g' = trackballGlobe g (0.1, 0.2) (0.4, -0.1)
                      shown s = screenToSphere g s >>= sphereToScreen g'
                      lift (x, y) = (x, y, sqrt (1 - x * x - y * y))
                      dist (a, b, c) (d, e, f) = sqrt ((a - d)^(2 :: Int) + (b - e)^(2 :: Int) + (c - f)^(2 :: Int))
                  in case (shown (0, 0), shown (0.2, 0.1)) of
                       (Just p, Just q) -> dist (lift p) (lift q) `shouldSatisfy` (\d -> abs (d - dist (lift (0, 0)) (lift (0.2, 0.1))) < 1e-9)
                       _ -> pure ()
                | g <- globes ]

    it "interpolates between views, ending at each" $
      sequence_ [ do screenToPlane (slerpGlobe g1 g2 0) (0.2, 0.1) `shouldSatisfy` closeTo (screenToPlane g1 (0.2, 0.1))
                     screenToPlane (slerpGlobe g1 g2 1) (0.2, 0.1) `shouldSatisfy` closeTo (screenToPlane g2 (0.2, 0.1))
                | g1 <- take 4 globes, g2 <- drop 4 globes ]

    it "is the Möbius map the kernel applies" $
      sequence_ [ let (a, b, c, d) = globeMobius g
                      u = x :+ y
                      v = (1 + sqrt (1 - x * x - y * y)) :+ 0
                      z = (a * u + b * v) / (c * u + d * v)
                  in screenToSphere g (x, y) `shouldSatisfy` maybe False (close3' (planeToSphere (toPair z)))
                | g <- globes, (x, y) <- [(0, 0), (0.3, -0.4), (-0.7, 0.2)] ]

    it "keeps the point under the pointer there while dragging" $
      sequence_ [ case screenToSphere g from of
                    Nothing -> expectationFailure "off the globe"
                    Just p -> sphereToScreen (trackballGlobe g from to) p
                                `shouldSatisfy` maybe False (\s -> close2' s to)
                | g <- globes, (from, to) <- [((0, 0), (0.3, 0.1)), ((0.2, -0.4), (-0.5, 0.3))] ]

    it "interpolates between rotations, ending at each" $ do
      let a = rotationFacing (1, 1)
          b = rotationFacing (-2, 0.5)
      rotateBy (slerp a b 0) (0, 0, 1) `shouldSatisfy` close3 (rotateBy a (0, 0, 1))
      rotateBy (slerp a b 1) (0, 0, 1) `shouldSatisfy` close3 (rotateBy b (0, 0, 1))

  describe "Jump to view's settings" $ do

    let wh = (600, 400)
        sameSettings a b = case (a, b) of
          (ViewSettings (x1, y1) t1 p1, ViewSettings (x2, y2) t2 p2) ->
            near x1 x2 && near y1 y2 && maybe (t2 == Nothing) (\t -> maybe False (nearAngle t) t2) t1
            && abs (p1 / p2 - 1) < 1e-9
        near a b = (isInfinite a && isInfinite b) || abs (a - b) <= 1e-9 * max 1 (abs a)
        nearAngle a b = let d = (a - b) / 360 in abs (d - fromIntegral (round d :: Int)) < 1e-9
        roundTrip m vs = case settingsView wh m vs of
          Right m' -> viewSettings wh m' `shouldSatisfy` sameSettings vs
          Left err -> expectationFailure err

    it "round-trip on the plane" $
      roundTrip (Model (0, 0) (0.01, 0.02) FlatShape) (ViewSettings (-0.75, 0.1) Nothing 0.0001)

    it "round-trip on the sphere, ∞ included" $ do
      let m = fitGlobe wh (Model (0, 0) (1, 1) (GlobeShape noGlobe))
      roundTrip m (ViewSettings (1.5, -2) (Just 30) 0.0001)
      roundTrip m (ViewSettings (-1e9, 3e9) (Just (-100)) 1e-12)
      roundTrip m (ViewSettings (1 / 0, 0) (Just 45) 0.001)
      roundTrip m (ViewSettings (1 / 0, 0) (Just 30) 0.00001)
      roundTrip (fitGlobe wh (Model (0, 0) (1, 1) (GlobeShape (globeFacing (-1.5, 0))))) (ViewSettings (1 / 0, 0) (Just 30) 0.00001)

    it "round-trip in the hyperbolic plane" $ do
      roundTrip (Model (0, 0) (0.005, 0.005) (HyperbolicShape DiskModel DiskDisplay identityIsometry))
                (ViewSettings (0.3, -0.4) (Just 60) 0.002)
      roundTrip (Model (0, 0.5) (0.005, 0.005) (HyperbolicShape HalfPlaneModel HalfPlaneDisplay identityIsometry))
                (ViewSettings (1.5, 2) (Just (-20)) 0.005)

  describe "a sphere viewer's header" $ do

    it "reads `view=sphere`, with the plane as the default" $ do
      case parseScript "@viewer \"V\" var=z view=sphere\npass\n" of
        Right [ViewerStanza{..}] -> vsView `shouldBe` SphereView
        _ -> expectationFailure "expected one viewer"
      case parseScript "@viewer \"V\"\npass\n" of
        Right [ViewerStanza{..}] -> vsView `shouldBe` PlaneView
        _ -> expectationFailure "expected one viewer"

    let headerError hdr needle = case parseScript (hdr ++ "\npass\n") of
          Left (_, err) -> err `shouldSatisfy` isInfixOf needle
          Right _ -> expectationFailure ("expected an error for " ++ hdr)

    it "rejects other views, and real viewers on the sphere" $ do
      headerError "@viewer \"V\" view=torus" "`plane`, `sphere`, `disk`, or `halfplane`"
      headerError "@viewer \"V\" xvar=x yvar=y view=sphere" "Only a complex viewer"

    it "builds a sphere viewer, whose point is a point of Ĉ" $ do
      checkScript "@viewer \"V\" var=z view=sphere center=\"1 + i\"\nz <- z^2\ncolor <- if z = ∞ then red else blue\n" `shouldBe` Right ()
      checkScript "@config \"P\"\nC : Ĉ := 0\n\n@viewer \"V\" var=C view=sphere\nz := 0\niterate z -> z^2 + C until z escapes\n"
        `shouldBe` Right ()
      checkScript "@viewer \"V\" var=z view=sphere\nw : C := z\ncolor <- red\n"
        `shouldSatisfy` either (("`z` is a point of the Riemann sphere" `isInfixOf`) . snd) (const False)
      checkScript "@config \"P\"\nC : C := 0\n\n@viewer \"V\" var=C view=sphere\ncolor <- red\n"
        `shouldSatisfy` either (("declare it `C : Ĉ`" `isInfixOf`) . snd) (const False)

    it "gives a sphere viewer's tools points of Ĉ, drawn where they are" $
      checkScript "@viewer \"V\" var=z view=sphere\ncolor <- red\n\n@tool \"T\" for=\"V\"\non click:\n  w := z^2\n  draw point at w\n  if w = ∞:\n    draw line from z to w\n"
        `shouldBe` Right ()
  where
    -- (dragging loses a little precision near the rim)
    close2' (a, b) (c, d) = abs (a - c) < 1e-6 && abs (b - d) < 1e-6
    toPair (a :+ b) = (a, b)
    closeTo a b = case (a, b) of
      (Just p, Just q) -> close3' (planeToSphere p) (planeToSphere q)
      _ -> a == b
    close3' (a, b, c) (d, e, f) = abs (a - d) < 1e-9 && abs (b - e) < 1e-9 && abs (c - f) < 1e-9

