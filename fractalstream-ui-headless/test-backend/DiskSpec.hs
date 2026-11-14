-- | Disk viewers (@view=disk@): the hyperbolic plane in the Poincaré disk,
-- moved by the view (an isometry), end to end.
--
-- (Plain strings, not a quasiquote; see "OverlaySpec".)
module DiskSpec (spec) where

import Test.Hspec

import Actor.Ensemble (buildEnsembleFromScript)
import Backend (withBackend)
import Interpreter (runScenario)
import Scenario (parseScenario)

project :: String
project = unlines
  [ "@viewer \"D\" view=disk center=0.5 pixelSize=1/250"
  -- red around the model's origin, blue around 0.5, grey elsewhere
  , "color <- if |z| < 0.02 then red else if |z - 0.5| < 0.02 then blue else grey"
  , ""
  , "@viewer \"P\" view=disk center=0.6 pixelSize=1/100"
  -- the pixel size in the red channel: at the screen's middle, it should be
  -- the local model width there, (1 - |0.6|²) / 100 = 0.0064, so 1/2
  , "color <- rgb(pixel_size / 0.0128, 0, 0)"
  , ""
  , "@tool \"T\" for=\"D\""
  , "on click at p:"
  , "  erase"
  , "  use yellow for fill"
  , "  use yellow for stroke"
  , "  draw filled circle at 0 with radius 0.05"
  , ""
  -- a marker 10 screen pixels in radius, wherever its point is
  , "@tool \"M\" for=\"D\""
  , "on click at p:"
  , "  erase"
  , "  use yellow for fill"
  , "  use yellow for stroke"
  , "  draw filled circle at 0 with radius 10 pixel_size_at(0)"
  , ""
  , "@viewer \"F\" center=0 pixelSize=1/100"
  , "color <- grey"
  , ""
  , "@tool \"N\" for=\"F\""
  , "on click at p:"
  , "  erase"
  , "  use yellow for fill"
  , "  use yellow for stroke"
  , "  draw filled circle at 0.5 with radius 10 pixel_size_at(0.5)"
  ]

-- | Run scenario commands against 'project'.
scenario :: [String] -> Expectation
scenario cmds = do
  ens <- buildEnsembleFromScript project >>= either (fail . snd) pure
  commands <- either (fail . snd) pure (parseScenario (unlines cmds))
  withBackend (\backend -> runScenario backend ens commands) >>= either expectationFailure pure

pixel :: String -> (Int, Int) -> String -> String
pixel viewer (x, y) rgb = "assert viewer \"" ++ viewer ++ "\" pixel (" ++ show x ++ ", " ++ show y ++ ") is " ++ rgb ++ " within 8"

red, blue, grey, background, yellow :: String
red = "(255, 0, 0)"
blue = "(0, 0, 255)"
grey = "(128, 128, 128)"
background = "(48, 48, 48)"
yellow = "(255, 255, 0)"

spec :: Spec
spec = describe "a disk viewer" $ do

  it "shows its `center=` in the middle of the disk" $ scenario
    [ "select viewer \"D\"", pixel "D" (300, 300) blue ]

  it "shows nothing off the disk" $ scenario
    [ "select viewer \"D\"", pixel "D" (2, 2) background ]

  it "moves its view to center a point (`set viewer center`)" $ scenario
    [ "select viewer \"D\"", "set viewer center to 0", pixel "D" (300, 300) red ]

  it "moves its view by a hyperbolic translation when dragged" $ scenario
    -- the origin goes to where 0.3 was shown, 0.3 of the disk's radius
    -- (250 pixels) right of the middle, starting from the view facing 0
    [ "select viewer \"D\"", "set viewer center to 0", pixel "D" (300, 300) red
    , "drag view from 0 to 0.3", pixel "D" (375, 300) red, pixel "D" (300, 300) grey ]

  it "can have its drawing's middle elsewhere (`set viewer screen center`)" $ scenario
    -- facing 0, the drawing's point 0.5 is the model point 0.5
    [ "select viewer \"D\"", "set viewer center to 0", "set viewer screen center to 0.5"
    , pixel "D" (300, 300) blue ]

  it "gives its code the pixel's model width as `pixel_size`" $ scenario
    [ "select viewer \"P\"", pixel "P" (300, 300) "(128, 0, 0)" ]

  it "draws its tools' drawings where the view puts their model points" $ scenario
    -- the origin is left of the middle, facing 0.5: at -0.5 moved back by
    -- the view, s = (0 - 0.5) / (1 - 0) = -0.5, 125 pixels left
    [ "select viewer \"D\"", "select tool \"T\"", "click at 0.5"
    , pixel "D" (175, 300) yellow ]

  it "sizes its tools' markers in screen pixels with `pixel_size_at`" $ scenario
    -- the origin's on the screen at (175, 300), where the view shrinks the
    -- model by 4/3: a marker 10 pixels in radius there
    [ "select viewer \"D\"", "select tool \"M\"", "click at 0.5"
    , pixel "D" (175 + 8, 300) yellow, pixel "D" (175 - 8, 300) yellow
    , pixel "D" (175, 300 + 8) yellow
    , pixel "D" (175 + 12, 300) grey, pixel "D" (175, 300 - 12) grey ]

  it "has `pixel_size_at` the pixel size on the plane" $ scenario
    [ "select viewer \"F\"", "select tool \"N\"", "click at 0"
    , pixel "F" (350 + 8, 300) yellow, pixel "F" (350 + 12, 300) grey ]
