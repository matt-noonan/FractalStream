-- | A viewer's overlay (@\@tool overlay@), end to end: its drawings are
-- shown whatever tool is selected (or none), kept up to date with the
-- configuration, and drawn above the viewer but below every tool's.
--
-- (Plain strings, not a quasiquote: this module links the backend, and
-- Template Haskell would load it at compile time; see the LLVM backend's
-- own spec.)
module OverlaySpec (spec) where

import Test.Hspec

import Actor.Ensemble (buildEnsembleFromScript)
import Backend (withBackend)
import Interpreter (runScenario)
import Scenario (parseScenario)

project :: String
project = unlines
  [ "@config \"Configuration\""
  , "p : C <- 0"
  , ""
  , "@viewer \"V\""
  , "color <- black"
  , ""
  , "@tool overlay for=\"V\""
  , "on refresh:"
  , "  erase"
  , "  use rgb(1, 0, 0) for fill"
  , "  draw filled rectangle from -1 - i to 1 + i"
  , "  use rgb(0, 1, 0) for fill"
  , "  draw filled circle at p with radius 10 pixel_size"
  , ""
  , "@tool \"Above\" for=\"V\" drawLayer=-1000"
  , "on refresh:"
  , "  erase"
  , "  use rgb(0, 0, 1) for fill"
  , "  draw filled rectangle from -1/2 - i/2 to 1/2 + i/2"
  , "on deactivated:"
  , "  erase"
  ]

-- | Run scenario commands against 'project'.
scenario :: [String] -> Expectation
scenario cmds = do
  ens <- buildEnsembleFromScript project >>= either (fail . snd) pure
  commands <- either (fail . snd) pure (parseScenario (unlines cmds))
  withBackend (\backend -> runScenario backend ens commands) >>= either expectationFailure pure

-- (the viewer is 600x600 with 0 in the middle, 128 pixels to a unit)
isColor :: (Int, Int) -> String -> String
isColor (x, y) rgb = "assert viewer \"V\" pixel (" ++ show x ++ ", " ++ show y ++ ") is " ++ rgb

red, green, blue, black :: String
red = "(255, 0, 0)"
green = "(0, 255, 0)"
blue = "(0, 0, 255)"
black = "(0, 0, 0)"

spec :: Spec
spec = describe "a viewer's overlay" $ do

  it "is drawn over the viewer with no tool selected" $ scenario
    [ isColor (300, 300) green
    , isColor (200, 300) red
    , isColor (20, 300) black
    ]

  it "is redrawn when the configuration changes" $ scenario
    [ "set parameter p to 1/2"
    , isColor (364, 300) green
    , isColor (300, 300) red
    ]

  it "is redrawn for the current view" $ scenario
    [ "select viewer \"V\""
    , "set viewer center to 1/2"
    , isColor (300, 300) red
    , isColor (236, 300) green
    ]

  it "is drawn below a tool's drawings, whatever the tool's layer" $ scenario
    [ "select viewer \"V\""
    , "select tool \"Above\""
    , "trigger refresh"
    , isColor (300, 300) blue
    , isColor (200, 300) red
    , "deselect tool"
    , isColor (300, 300) green
    ]
