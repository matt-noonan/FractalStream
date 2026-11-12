-- | @autocolor@ in a tool's handler: each point gets a color of its own,
-- the same one each time (a compiled handler that meets a point to add is
-- run again once it's added, in the wasm backend).
--
-- (Plain strings, not a quasiquote; see "OverlaySpec".)
module ToolAutocolorSpec (spec) where

import Test.Hspec

import Actor.Ensemble (buildEnsembleFromScript, ensembleFields)
import Actor.Layout (fieldName, fieldText)
import Backend (withBackend)
import Interpreter (runScenario)
import Scenario (parseScenario)

project :: String
project = unlines
  [ "@config \"Configuration\""
  , "k : Color := black"
  , ""
  , "@viewer \"V\""
  , "color <- grey"
  , ""
  , "@tool \"T\" for=\"V\""
  , "on click at p:"
  , "  k <- autocolor(0, p)"
  ]

-- | The color k after each of some clicks, from the start.
colorsAfter :: [String] -> IO [String]
colorsAfter clicks = do
  ens <- buildEnsembleFromScript project >>= either (fail . snd) pure
  withBackend $ \backend ->
    mapM (\c -> do
            -- (each run starts with nothing selected; the autocolor groups
            -- are the project's, and last)
            click <- either (fail . snd) pure (parseScenario (unlines
                       [ "select viewer \"V\"", "select tool \"T\"", "click at " ++ c ]))
            runScenario backend ens click >>= either fail pure
            fields <- ensembleFields ens
            sequence [ fieldText f | (_, f) <- fields, fieldName f == "k" ] >>= \case
              [t] -> pure t
              _ -> fail "no field k") clicks

spec :: Spec
spec = describe "autocolor in a tool" $
  it "gives each point a color of its own, the same each time" $ do
    [a, b, a'] <- colorsAfter ["0.5", "2 + i", "0.5"]
    a `shouldNotBe` "rgb(0.000, 0.000, 0.000)"
    a `shouldNotBe` b
    a' `shouldBe` a
