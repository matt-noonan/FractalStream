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

    it "gives `on click:` the point clicked, as `click`" $
      shouldParse [r|
on click:
  draw point at click
|]

    it "gives `on double-click:` the point clicked, as `click`" $
      shouldParse [r|
on double-click:
  w := click
  draw point at w
|]

    it "handles drags with `on click:` when there's no `on drag:`" $
      case runScript "on click:\n  draw point at click\n" of
        Left (_, err) -> expectationFailure err
        Right h -> (isJust (onClick h), isJust (onDrag h)) `shouldBe` (True, True)

    it "keeps `on click:` and `on drag:` apart when there are both" $
      shouldParse [r|
on click:
  draw point at click
on drag:
  draw line from drag start to drag end
|]

    it "gives `on drag:` its start and where the pointer is now" $
      shouldParse [r|
on drag:
  draw line from drag start to drag end
|]

    it "gives `on drag-finished:` its start and end" $
      shouldParse [r|
on drag-finished:
  draw line from drag start to drag end
|]

    it "says where `click`, `drag start` and `drag end` can be used" $ do
      [r|
on drag:
  draw point at click
|] `shouldFailWith` "only defined in a tool's `on click:`"
      [r|
on click:
  draw point at drag start
|] `shouldFailWith` "only defined in a tool's `on drag:` and `on drag-finished:`"
      [r|
on refresh:
  draw point at drag end
|] `shouldFailWith` "`drag end` is where a drag is now, or ended"

    it "doesn't take a name for an event's point, or `on click-or-drag:`" $ do
      isLeft (runScript "on click at w:\n  pass\n") `shouldBe` True
      isLeft (runScript "on drag from a to b:\n  pass\n") `shouldBe` True
      isLeft (runScript "on click-or-drag:\n  pass\n") `shouldBe` True

    it "doesn't let a script name a variable `click`" $
      isLeft (runScript "on refresh:\n  click := 1\n") `shouldBe` True

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

on click:
  use white for line
  erase
  draw point at click
|]

    it "reports a parse error for a malformed header" $
      case parseToolScript noCommonDefs (ComplexCoordinate "z") limits Nothing [r|
on clickety:
  pass
|] of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected a parse error for an unknown event keyword"
