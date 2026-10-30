{-# language QuasiQuotes #-}
module Actor.EventSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Environment
import Language.Code.Parser (noCommonDefs)
import Language.Value.Typecheck (ParsedValue)
import Language.Parser.SourceRange (SourceRange)

import Actor.Event
import Actor.Viewer (defaultIterLimit, defaultMaxRadius, defaultMinRadius)

import Data.List (isInfixOf)
import Text.RawString.QQ

-- | A minimal environment, with a single complex variable @z@ that serves
-- as the default click\/drag coordinate (as the viewer's z-coord would).
type TestEnv = '[ '("z", 'ComplexT) ]

testEnv :: EnvironmentProxy TestEnv
testEnv = BindingProxy (Proxy @"z") ComplexType EmptyEnvProxy

limits :: (ParsedValue, ParsedValue, ParsedValue)
limits = (defaultIterLimit, defaultMaxRadius, defaultMinRadius)

runScript :: String -> Either (SourceRange, String) (CombinedEventHandler TestEnv)
runScript src =
  case parseToolScript noCommonDefs (ComplexCoordinate "z") limits Nothing src of
    Left err     -> Left err
    Right script -> runParsedToolScript script testEnv

shouldParse :: String -> Expectation
shouldParse src = case runScript src of
  Left (_, err) -> expectationFailure ("expected success, got error: " ++ err)
  Right _       -> pure ()

shouldFailWith :: String -> String -> Expectation
shouldFailWith src needle = case runScript src of
  Left (_, err) -> err `shouldSatisfy` isInfixOf needle
  Right _       -> expectationFailure "expected a parse/typecheck error, but it succeeded"

spec :: Spec
spec = do

  describe "parseToolScript" $ do

    it "accepts an empty script" $
      shouldParse ""

    it "parses a single on-refresh block" $
      shouldParse [r|
on refresh:
  pass
|]

    it "parses activated/deactivated blocks" $
      shouldParse [r|
on activated:
  pass
on deactivated:
  pass
|]

    it "rejects duplicate refresh blocks" $
      [r|
on refresh:
  pass
on refresh:
  pass
|] `shouldFailWith` "Duplicate definitions of the refresh event handler"

    it "parses a click block with an explicit coordinate" $
      shouldParse [r|
on click at w:
  pass
|]

    it "parses a click block with the default coordinate" $
      shouldParse [r|
on click:
  draw point at z
|]

    it "parses a double-click block" $
      shouldParse [r|
on double-click:
  pass
|]

    it "parses a click-or-drag block, building both handlers" $
      shouldParse [r|
on click-or-drag at z:
  draw point at z
|]

    it "parses a drag block with explicit start/current coordinates" $
      shouldParse [r|
on drag from a to b:
  draw line from a to b
|]

    it "parses a drag block with the default coordinate" $
      shouldParse [r|
on drag:
  pass
|]

    it "parses a drag-finished block" $
      shouldParse [r|
on drag-finished from a to b:
  draw line from a to b
|]

    it "parses a button block" $
      shouldParse [r|
on button "Go":
  pass
|]

    it "rejects duplicate button names" $
      [r|
on button "Go":
  pass
on button "Go":
  erase
|] `shouldFailWith` "Duplicate definitions of the button event handler named `Go`"

    it "parses a timer block" $
      shouldParse [r|
on timer "tick" every 500 ms:
  pass
|]

    it "parses multiple distinct blocks together, as in a typical tool" $
      shouldParse [r|
on refresh:
  erase

on deactivated:
  erase

on click-or-drag at z:
  use white for line
  erase
  draw point at z
|]

    it "reports a parse error for a malformed header" $
      case parseToolScript noCommonDefs (ComplexCoordinate "z") limits Nothing [r|
on clickety at z:
  pass
|] of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected a parse error for an unknown event keyword"
