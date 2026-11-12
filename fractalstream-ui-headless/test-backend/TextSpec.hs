-- | Text in a tool's handler: made from literals, numbers (formatted as
-- the interpreter formats them, whichever backend runs it), and other
-- texts; read from, and written back to, the configuration's text and
-- list-of-text fields.
--
-- (Plain strings, not a quasiquote; see "OverlaySpec".)
module TextSpec (spec) where

import Test.Hspec

import Actor.Ensemble (buildEnsembleFromScript, ensembleFields)
import Actor.Layout (fieldName, fieldText)
import Backend (withBackend)
import Interpreter (runScenario)
import Scenario (parseScenario)

project :: String
project = unlines
  [ "@config \"Configuration\""
  , "t : Text := \"start\""
  , "u : Text := \"\""
  , "ts : List of Text := [\"a\", \"bé\"]"
  , "n : Z := 0"
  , ""
  , "@viewer \"V\""
  , "color <- grey"
  , ""
  , "@tool \"T\" for=\"V\""
  , "on click at z:"
  , "  u <- text(t, \"; z = \", z, \", k = \", 3, \", r = \", 0.01, \", big = \", 10000000000.0, \", neg = \", -2.5, \" ünï ✓\")"
  , "  n <- length(ts)"
  , "  ts <- [text(\"x\", n), \"\", text(\"\", z)]"
  , "  t <- if n > 1 then \"many\" else \"few\""
  , "  write text(\"at \", z) at z"
  ]

-- | Run scenario commands against 'project', then give each field's text.
scenario :: [String] -> IO [(String, String)]
scenario cmds = do
  ens <- buildEnsembleFromScript project >>= either (fail . snd) pure
  commands <- either (fail . snd) pure (parseScenario (unlines cmds))
  withBackend (\backend -> runScenario backend ens commands) >>= either fail pure
  fields <- ensembleFields ens
  mapM (\(_, f) -> (fieldName f,) <$> fieldText f) fields

spec :: Spec
spec = describe "text in a tool" $
  it "is made, read, and written back as the interpreter does" $ do
    values <- scenario [ "select viewer \"V\"", "select tool \"T\"", "click at (0.5, -0.25)" ]
    lookup "u" values `shouldBe`
      Just "start; z = 0.5 + -0.25i, k = 3, r = 1.0e-2, big = 1.0e10, neg = -2.5 ünï ✓"
    lookup "n" values `shouldBe` Just "2"
    lookup "t" values `shouldBe` Just "many"
    lookup "ts" values `shouldBe` Just "\"x2\", \"\", \"0.5 + -0.25i\""
