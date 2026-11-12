-- | A scenario's @drag from A to B@ reaches a tool's drag handlers as the
-- live app's drags do: @on drag from p to q:@ gets where the drag
-- started as @p@, and where the pointer is as @q@.
--
-- (Plain strings, not a quasiquote; see "OverlaySpec".)
module DragSpec (spec) where

import Test.Hspec

import Actor.Ensemble (buildEnsembleFromScript)
import Backend (withBackend)
import Interpreter (runScenario)
import Scenario (parseScenario)

project :: String
project = unlines
  [ "@config \"Configuration\""
  , "start : C := 0"
  , "now : C := 0"
  , "start2 : C := 0"
  , "now2 : C := 0"
  , ""
  , "@viewer \"V\""
  , "color <- grey"
  , ""
  , "@viewer \"W\""
  , "color <- if start = 1 and now = 2 + i then rgb(0, 1, 0) else rgb(1, 0, 0)"
  , ""
  , "@viewer \"F\""
  , "color <- if start2 = 1 and now2 = 2 + i then rgb(0, 1, 0) else rgb(1, 0, 0)"
  , ""
  , "@tool \"T\" for=\"V\""
  , "on drag from p to q:"
  , "  start <- p"
  , "  now <- q"
  , "on drag-finished from p to q:"
  , "  start2 <- p"
  , "  now2 <- q"
  ]

scenario :: [String] -> Expectation
scenario cmds = do
  ens <- buildEnsembleFromScript project >>= either (fail . snd) pure
  commands <- either (fail . snd) pure (parseScenario (unlines cmds))
  withBackend (\backend -> runScenario backend ens commands) >>= either expectationFailure pure

isGreen :: String -> String
isGreen viewer = "assert viewer \"" ++ viewer ++ "\" pixel (10, 10) is (0, 255, 0)"

spec :: Spec
spec = describe "a scenario's drag" $
  it "gives a tool's drag handlers where it started, then where it's got to" $ scenario
    [ "select viewer \"V\"", "select tool \"T\""
    , "drag from (1, 0) to (2, 1)"
    , isGreen "W", isGreen "F" ]
