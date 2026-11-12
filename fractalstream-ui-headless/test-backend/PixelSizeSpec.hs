-- | A viewer's code sees its pixel's size (@pixel_size@, or @pixel_width@
-- and @pixel_height@) as the viewer's pixel, even when it's rendered
-- smoothed (sampled at twice the size), as fsrun's captures and
-- assertions are, and as the desktop app's viewers see it.
--
-- (Plain strings, not a quasiquote; see "OverlaySpec".)
module PixelSizeSpec (spec) where

import Test.Hspec

import Actor.Ensemble (buildEnsembleFromScript)
import Backend (withBackend)
import Interpreter (runScenario)
import Scenario (parseScenario)

project :: String
project = unlines
  [ "@viewer \"Z\" center=0 pixelSize=1/128"
  , "color <- if pixel_size = 1/128 then rgb(0, 1, 0) else rgb(1, 0, 0)"
  , ""
  , "@viewer \"R\" xvar=x yvar=y center=(0, 0) pixelSize=1/64 aspect=2"
  , "color <- if pixel_width = 1/64 and pixel_height = 1/32 then rgb(0, 1, 0) else rgb(1, 0, 0)"
  ]

scenario :: [String] -> Expectation
scenario cmds = do
  ens <- buildEnsembleFromScript project >>= either (fail . snd) pure
  commands <- either (fail . snd) pure (parseScenario (unlines cmds))
  withBackend (\backend -> runScenario backend ens commands) >>= either expectationFailure pure

spec :: Spec
spec = describe "a viewer's pixel size" $
  it "is its pixel's, though it's rendered smoothed" $ scenario
    [ "assert viewer \"Z\" pixel (10, 10) is (0, 255, 0)"
    , "assert viewer \"R\" pixel (10, 10) is (0, 255, 0)" ]
