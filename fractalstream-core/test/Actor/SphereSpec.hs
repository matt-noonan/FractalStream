-- | Sphere viewers: the geometry of the globe, and @view=sphere@.
module Actor.SphereSpec (spec) where

import Test.Hspec

import Actor.Script
import Actor.Event (ViewKind(..))
import Actor.Ensemble (checkScript)
import Actor.Viewer.Sphere

import Data.List (isInfixOf)

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
      sequence_ [ fmap (sphereToScreen r) (screenToSphere r s) `shouldSatisfy` maybe False (maybe False (close2 s))
                | r <- rotations, s <- [(0, 0), (0.5, 0.5), (-0.9, 0.1), (0, -0.99)] ]

    it "shows nothing off the globe, or on its far side" $ do
      screenToSphere noRotation (0.8, 0.8) `shouldBe` Nothing
      planeToScreen noRotation (1e6, 0) `shouldBe` Nothing

    it "faces 0 with no rotation, the real axis to the right and the imaginary axis up" $ do
      screenToPlane noRotation (0, 0) `shouldSatisfy` maybe False (close2 (0, 0))
      planeToScreen noRotation (0.1, 0) `shouldSatisfy` maybe False (\(x, y) -> x > 0 && close y 0)
      planeToScreen noRotation (0, 0.1) `shouldSatisfy` maybe False (\(x, y) -> close x 0 && y > 0)

    it "turns the globe to face a point" $
      mapM_ (\z -> screenToPlane (rotationFacing z) (0, 0) `shouldSatisfy` maybe False (close2 z))
            [ p | p <- points, p /= (0, 0) ]

    it "keeps the point under the pointer there while dragging" $
      sequence_ [ case screenToSphere r from of
                    Nothing -> expectationFailure "off the globe"
                    Just p -> sphereToScreen (trackballRotation r from to) p
                                `shouldSatisfy` maybe False (\s -> close2' s to)
                | r <- rotations, (from, to) <- [((0, 0), (0.3, 0.1)), ((0.2, -0.4), (-0.5, 0.3))] ]

    it "interpolates between rotations, ending at each" $ do
      let a = rotationFacing (1, 1)
          b = rotationFacing (-2, 0.5)
      rotateBy (slerp a b 0) (0, 0, 1) `shouldSatisfy` close3 (rotateBy a (0, 0, 1))
      rotateBy (slerp a b 1) (0, 0, 1) `shouldSatisfy` close3 (rotateBy b (0, 0, 1))

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
      headerError "@viewer \"V\" view=torus" "`plane` or `sphere`"
      headerError "@viewer \"V\" xvar=x yvar=y view=sphere" "Only a complex viewer"

    it "builds a sphere viewer, and keeps a complex viewer's names free" $ do
      checkScript "@viewer \"V\" var=z view=sphere center=\"1 + i\"\nz <- z^2\ncolor <- red\n" `shouldBe` Right ()
      checkScript "@config \"P\"\nC : C <- 0\n\n@viewer \"V\" var=C view=sphere\nz := 0\niterate z -> z^2 + C until z escapes\n"
        `shouldBe` Right ()
  where
    -- (dragging loses a little precision near the rim)
    close2' (a, b) (c, d) = abs (a - c) < 1e-6 && abs (b - d) < 1e-6
