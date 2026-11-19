-- | A sphere viewer's tools (@view=sphere@), end to end: their events and
-- their drawings are at points of the Riemann sphere, ∞ included, as
-- homogeneous coordinates, exactly.
--
-- (Plain strings, not a quasiquote; see "OverlaySpec".)
module SphereDrawSpec (spec) where

import Test.Hspec

import Actor.Ensemble (buildEnsembleFromScript)
import Backends (withBackend)
import Interpreter (runScenario)
import Scenario (parseScenario)
import Data.List (isInfixOf)

-- | A globe facing 10⁹, unzoomed: ∞ in the middle pixel (10⁻⁹ of a
-- radian off; a header's `center=` can't be ∞ itself), the unit circle
-- its rim, 0.46 of the window (276 pixels) in radius.
project :: String -> String
project tool = unlines $
  [ "@viewer \"S\" view=sphere center=1000000000"
  , "color <- grey"
  , ""
  , "@tool \"T\" for=\"S\""
  , "on click at p:"
  , "  erase"
  ] ++ map ("  " ++) (lines tool)

scenario :: String -> [String] -> Expectation
scenario tool cmds = do
  ens <- buildEnsembleFromScript (project tool) >>= either (fail . snd) pure
  commands <- either (fail . snd) pure (parseScenario (unlines ("select viewer \"S\"" : "select tool \"T\"" : cmds)))
  withBackend (\backend -> runScenario backend ens commands) >>= either expectationFailure pure

pixel :: (Int, Int) -> String -> String
pixel (x, y) rgb = "assert viewer \"S\" pixel (" ++ show x ++ ", " ++ show y ++ ") is " ++ rgb ++ " within 40"

red, yellow, grey :: String
red = "(255, 0, 0)"
yellow = "(255, 255, 0)"
grey = "(128, 128, 128)"

spec :: Spec
spec = describe "a sphere viewer's tools" $ do

  it "are given ∞ itself, and draw at it" $ scenario
    (unlines [ "if p = ∞:", "  use yellow for fill", "  use yellow for stroke"
             , "  draw filled circle at p with radius 6 pixels" ])
    [ "click at ∞", pixel (300, 300) yellow, pixel (300 + 20, 300) grey ]

  it "draw a circle by its chordal radius" $ scenario
    -- (chordal 1 is 60° from ∞: sin 60° of the globe's radius, 239 pixels)
    (unlines [ "use red for stroke", "draw circle at p with radius 1" ])
    -- (a plane radius of 1, about a point 10⁹ out, would be a speck in
    -- the middle; the globe's shading darkens it away from the light, so
    -- only the middle is the viewer's own grey)
    [ "click at ∞", pixel (300 + 239, 300) red, pixel (300, 300 - 239) red
    , pixel (300 - 239, 300) red, pixel (300, 300) grey ]

  it "draw a line as an arc of a great circle, through ∞" $ scenario
    -- (from ∞, in the middle, out along the real axis both ways)
    (unlines [ "use red for stroke", "draw line from p to 2", "draw line from p to -2" ])
    [ "click at ∞", pixel (300 + 100, 300) red, pixel (300 - 100, 300) red ]

  it "can't draw rectangles" $ do
    built <- buildEnsembleFromScript (project "draw rectangle from 0 to 1")
    either (("can't draw rectangles" `isInfixOf`) . snd) (const False) built `shouldBe` True
