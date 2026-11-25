-- | A viewer's @show if:@, in a scenario: a hidden viewer can't be drawn
-- (a capture or assert of it says why), and once a tool makes its
-- @show if:@ hold, it's drawn as usual.
--
-- (Plain strings, not a quasiquote; see "OverlaySpec".)
module ShowIfSpec (spec) where

import Test.Hspec

import Data.List (isInfixOf)

import Actor.Ensemble (buildEnsembleFromScript)
import Backends (withBackend)
import Interpreter (runScenario)
import Scenario (parseScenario)

project :: String
project = unlines
  [ "@config \"Parameters\""
  , "C : C := 0"
  , "selected_C : Bool := false"
  , ""
  , "@viewer \"Mandelbrot set\" over C"
  , "z := 0"
  , "iterate z -> z^2 + C until z escapes"
  , "color <- if z escaped then white else black"
  , ""
  , "@viewer \"Julia set\"\nsetup:\n  show if: selected_C"
  , "iterate z -> z^2 + C until z escapes"
  , "color <- if z escaped then white else black"
  , ""
  , "@tool \"Pick C\" for \"Mandelbrot set\""
  , "on click:"
  , "  C <- click"
  , "  selected_C <- true"
  ]

scenario :: [String] -> IO (Either String ())
scenario cmds = do
  ens <- buildEnsembleFromScript project >>= either (fail . snd) pure
  commands <- either (fail . snd) pure (parseScenario (unlines cmds))
  fmap (const ()) <$> withBackend (\backend -> runScenario backend ens commands)

-- (the middle of the filled Julia set, for C = -0.1)
juliaMiddleIsBlack :: String
juliaMiddleIsBlack = "assert viewer \"Julia set\" pixel (300, 300) is (0, 0, 0)"

spec :: Spec
spec = describe "a viewer's show_if=" $ do
  it "keeps a hidden viewer from being drawn, saying why" $
    scenario [juliaMiddleIsBlack] >>= \case
      Left err -> err `shouldSatisfy` isInfixOf "is hidden"
      Right () -> expectationFailure "drew a viewer whose show_if= doesn't hold"

  it "draws it once a tool makes its show_if= hold" $
    scenario [ "select viewer \"Mandelbrot set\"", "select tool \"Pick C\""
             , "click at -0.1", juliaMiddleIsBlack ]
      >>= either expectationFailure pure
