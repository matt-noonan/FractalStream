-- | The part of a viewer's code that's the same for every pixel, end to
-- end in the compiled backend: it's run once per render (see
-- "Language.Code.Hoist"), and the rest of the code, run for each pixel,
-- must come out just as if each pixel had run all of it.
--
-- (Plain strings, not a quasiquote; see "OverlaySpec".)
module HoistSpec (spec) where

import Test.Hspec

import Actor.Ensemble (buildEnsembleFromScript)
import Backend (withBackend)
import Interpreter (runScenario)
import Scenario (parseScenario)

project :: String
project = unlines
  [ "@config \"P\""
  , "a : R := 1"
  , ""
  -- `n` starts out the same for every pixel, and then each pixel changes
  -- it: no pixel should see another's change
  , "@viewer \"Each\" var=C"
  , "n := 0"
  , "n <- n + (if re(C) < 1000 then 1 else 2)"
  , "color <- if n = 1 then black else white"
  , ""
  -- a loop that's the same for every pixel leaves its iteration count,
  -- which code for each pixel reads before any loop of its own
  , "@viewer \"Count\" var=C"
  , "k := 0"
  , "while k < 7:"
  , "  k <- k + 1"
  , "color <- if iterations = 7 and |C| < 1000 then black else white"
  , ""
  -- a value worked out once, from the configuration, colors every pixel
  , "@viewer \"Config\" var=C"
  , "b := a^2"
  , "color <- rgb(b / 10, re(C) - re(C), 0)"
  ]

scenario :: [String] -> Expectation
scenario cmds = do
  ens <- buildEnsembleFromScript project >>= either (fail . snd) pure
  commands <- either (fail . snd) pure (parseScenario (unlines cmds))
  withBackend (\backend -> runScenario backend ens commands) >>= either expectationFailure pure

at :: String -> (Int, Int) -> String -> String
at viewer (x, y) rgb = "assert viewer \"" ++ viewer ++ "\" pixel (" ++ show x ++ ", " ++ show y ++ ") is " ++ rgb ++ " within 1"

spec :: Spec
spec = describe "work that's the same for every pixel" $ do

  it "is done once, and each pixel starts from its results" $ scenario
    [ "select viewer \"Each\""
    , at "Each" (0, 0) "(0, 0, 0)", at "Each" (17, 3) "(0, 0, 0)"
    , at "Each" (300, 300) "(0, 0, 0)", at "Each" (599, 599) "(0, 0, 0)" ]

  it "hands on the iteration count of its loops" $ scenario
    [ "select viewer \"Count\""
    , at "Count" (0, 0) "(0, 0, 0)", at "Count" (300, 300) "(0, 0, 0)" ]

  it "is done again when the configuration changes" $ scenario
    [ "select viewer \"Config\""
    , at "Config" (10, 10) "(26, 0, 0)"
    , "set parameter a to 3"
    , at "Config" (10, 10) "(230, 0, 0)" ]
