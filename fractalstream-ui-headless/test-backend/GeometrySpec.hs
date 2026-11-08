-- | A viewer's geometry (@viewer_width@, @viewer_height@, and
-- @viewer_center@, or a real viewer's @viewer_center_x@ and
-- @viewer_center_y@), as its code and its tools read it: kept up to
-- date as the viewer is resized and moved.
--
-- (Plain strings, not a quasiquote; see "OverlaySpec".)
module GeometrySpec (spec) where

import Test.Hspec

import Actor.Ensemble (buildEnsembleFromScript)
import Backend (withBackend)
import Interpreter (runScenario)
import Scenario (parseScenario)

project :: String
project = unlines
  [ "@config \"Configuration\""
  , "w : Z := 0"
  , "c : C := 0"
  , ""
  , "@viewer \"V\" center=\"1 + i\""
  , "color <- if viewer_width = 300 and viewer_height = 200 and viewer_center = 1 + i then rgb(0, 1, 0) else rgb(1, 0, 0)"
  , ""
  , "@viewer \"R\" xvar=x yvar=y center=(2, 3)"
  , "color <- if viewer_width = 300 and viewer_center_x = 2 and viewer_center_y = 3 then rgb(0, 1, 0) else rgb(1, 0, 0)"
  , ""
  , "@viewer \"W\""
  , "color <- if w = 500 and c = 5 then rgb(0, 1, 0) else rgb(1, 0, 0)"
  , ""
  , "@tool \"T\" for=\"V\""
  , "on click at p:"
  , "  w <- viewer_width + viewer_height"
  , "  c <- viewer_center"
  ]

-- | Run scenario commands against 'project'.
scenario :: [String] -> Expectation
scenario cmds = do
  ens <- buildEnsembleFromScript project >>= either (fail . snd) pure
  commands <- either (fail . snd) pure (parseScenario (unlines cmds))
  withBackend (\backend -> runScenario backend ens commands) >>= either expectationFailure pure

isColor :: String -> String -> String
isColor viewer rgb = "assert viewer \"" ++ viewer ++ "\" pixel (10, 10) is " ++ rgb

red, green :: String
red = "(255, 0, 0)"
green = "(0, 255, 0)"

spec :: Spec
spec = describe "a viewer's geometry" $ do

  it "is visible to a complex viewer's code, as it's resized and moved" $ scenario
    [ "select viewer \"V\"", isColor "V" red
    , "resize viewer to 300x200", isColor "V" green
    , "set viewer center to 5", isColor "V" red ]

  it "is visible to a real viewer's code" $ scenario
    [ "select viewer \"R\"", "resize viewer to 300x300", isColor "R" green
    , "set viewer center to (2, 4)", isColor "R" red ]

  it "is visible to the viewer's tools" $ scenario
    [ "select viewer \"V\"", "resize viewer to 300x200", "set viewer center to 5"
    , isColor "W" red
    , "select tool \"T\"", "click at 5", isColor "W" green ]
