{-# language QuasiQuotes #-}
module Actor.ScriptSpec (spec) where

import Test.Hspec

import Language.Type

import Actor.Script


import Data.List (isInfixOf)
import Text.RawString.QQ

runScript :: String -> Either String [ParsedStanza]
runScript src = case parseScript src of
  Left (_, err)  -> Left err
  Right stanzas  -> Right stanzas

shouldParseTo :: String -> ([ParsedStanza] -> Expectation) -> Expectation
shouldParseTo src check = case runScript src of
  Left err       -> expectationFailure ("expected success, got error: " ++ err)
  Right stanzas  -> check stanzas

shouldFailWith :: String -> String -> Expectation
shouldFailWith src needle = case runScript src of
  Left err -> err `shouldSatisfy` isInfixOf needle
  Right _  -> expectationFailure "expected a parse/typecheck error, but it succeeded"

typeName :: SomeType -> String
typeName (SomeType t) = ppType t

spec :: Spec
spec = do

  describe "parseScript" $ do

    it "accepts a single viewer stanza" $
      shouldParseTo [r|
@viewer "Mandelbrot Set" var=C
z : C <- 0
iterate z -> z^2 + C until z escapes
|] $ \case
        [ViewerStanza{..}] -> do
          vsTitle `shouldBe` "Mandelbrot Set"
          vsVar `shouldBe` "C"
          vsCenter `shouldBe` Nothing
          -- Omitted, so defaults to a reference to the automatically
          -- provided `max_iters` (see 'validateViewerHeader') --
          -- not `Nothing` anymore.
          vsIterLimit `shouldBe` Just "max_iters"
        other -> expectationFailure ("expected a single viewer stanza, got " ++ show (length other) ++ " stanzas")

    it "accepts optional viewer attributes" $
      shouldParseTo [r|
@viewer "Mandelbrot Set" var=C center="0" pixelSize="1/128" limit="maxIters" escape="maxRadius" vanish="minRadius" pixelVar=px
pass
|] $ \case
        [ViewerStanza{..}] -> do
          vsCenter `shouldBe` Just "0"
          vsPixelSize `shouldBe` Just "1/128"
          vsIterLimit `shouldBe` Just "maxIters"
          vsEscape `shouldBe` Just "maxRadius"
          vsVanish `shouldBe` Just "minRadius"
          vsPixelVar `shouldBe` Just "px"
        other -> expectationFailure ("expected a single viewer stanza, got " ++ show (length other) ++ " stanzas")

    it "defaults pixelVar to a reference to the automatically-provided \
       \`pixel_size`" $
      shouldParseTo [r|
@viewer "Mandelbrot Set" var=C
pass
|] $ \case
        [ViewerStanza{..}] -> do
          vsPixelVar `shouldBe` Just "pixel_size"
        other -> expectationFailure ("expected a single viewer stanza, got " ++ show (length other) ++ " stanzas")

    it "rejects the window-geometry attributes `size`, `position`, and \
       \`resizable`, pointing at where that state is saved now" $ do
      [r|
@viewer "Mandelbrot Set" var=C size=500x500
pass
|] `shouldFailWith` "`size` attribute is no longer part of a `@viewer` header"
      [r|
@viewer "Mandelbrot Set" var=C position="10x20"
pass
|] `shouldFailWith` "wx-properties"
      [r|
@viewer "Mandelbrot Set" var=C resizable=false
pass
|] `shouldFailWith` "wx-properties"

    it "defaults a viewer's `var` to `z` when the attribute is omitted" $
      shouldParseTo [r|
@viewer "Mandelbrot Set"
pass
|] $ \case
        [ViewerStanza{..}] -> do
          vsVar `shouldBe` "z"
        other -> expectationFailure ("expected a single viewer stanza, got " ++ show (length other) ++ " stanzas")

    it "accepts unquoted attribute values: a whole number (negative \
       \too), a whole-number fraction, and a boolean" $
      shouldParseTo [r|
@viewer "Mandelbrot Set" var=C center=-2 pixelSize=1/128
pass


@tool "Demo" drawLayer=-1 refreshOnActivate=false
on click:
  pass
|] $ \case
        [ViewerStanza{..}, ToolStanza{..}] -> do
          vsCenter `shouldBe` Just "-2"
          vsPixelSize `shouldBe` Just "1/128"
          tsDrawLayer `shouldBe` (-1)
          tsRefreshOnActivate `shouldBe` False
        other -> expectationFailure ("expected a viewer and a tool stanza, got " ++ show (length other) ++ " stanzas")

    it "accepts an unquoted decimal attribute value, rendered back \
       \without ever using scientific notation (regression: show \
       \(0.0001 :: Double) == \"1.0e-4\", which re-parses as 1.0 * e - 4, \
       \since `e` is the Euler's-number keyword)" $
      shouldParseTo [r|
@viewer "Mandelbrot Set" center=-0.5 pixelSize=0.0001
pass
|] $ \case
        [ViewerStanza{..}] -> do
          vsCenter `shouldBe` Just "-0.5"
          vsPixelSize `shouldBe` Just "0.0001"
        other -> expectationFailure ("expected a single viewer stanza, got " ++ show (length other) ++ " stanzas")

    it "rejects a viewer stanza with an unknown attribute" $
      [r|
@viewer "Mandelbrot Set" var=C bogus="1"
pass
|] `shouldFailWith` "Unknown attribute `bogus`"

    it "rejects a viewer stanza with a duplicate attribute" $
      [r|
@viewer "Mandelbrot Set" var=C var=z
pass
|] `shouldFailWith` "Duplicate attribute `var`"



    it "accepts a config stanza with declarations and text" $
      shouldParseTo [r|
@config "Configuration"
maxIters : Z as "Iteration limit:" <- 100
text "Here is some text"
|] $ \case
        [ConfigStanza "Configuration"
          [ ConfigDecl{cdName = "maxIters", cdType, cdLabel = Just "Iteration limit:", cdChoices = Nothing}
          , ConfigText "Here is some text"
          ]] -> typeName cdType `shouldBe` "ℤ"
        other -> expectationFailure ("did not match expected config stanza: " ++ show (length other) ++ " stanzas")

    it "rejects a button in a config stanza, even inside a row or tab" $ do
      [r|
@config "Configuration"
button "Reset"
|] `shouldFailWith` "can't have a button"
      [r|
@config "Configuration"
tab "A":
  row:
    button "Reset"
|] `shouldFailWith` "can't have a button"

    it "decodes escape sequences in text" $
      shouldParseTo "@config \"C\"\ntext \"say \\\"hi\\\" \\\\ bye\"\n" $ \case
        [ConfigStanza "C" [ConfigText t]] -> t `shouldBe` "say \"hi\" \\ bye"
        other -> expectationFailure ("unexpected stanzas: " ++ show (length other))

    it "accepts a row: block grouping declarations side by side" $
      shouldParseTo [r|
@config "Configuration"
row:
  a : Z <- 1
  b : Z <- 2
c : Z <- 3
|] $ \case
        [ConfigStanza "Configuration"
          [ ConfigRow [ConfigDecl{cdName = "a"}, ConfigDecl{cdName = "b"}]
          , ConfigDecl{cdName = "c"}
          ]] -> pure ()
        other -> expectationFailure ("did not match expected config stanza: " ++ show (length other) ++ " stanzas")

    it "accepts text and button statements inside a tool's row:" $
      shouldParseTo [r|
@tool "T"
row:
  text "hi"
  button "Go"
on button "Go":
  pass
|] $ \case
        [ToolStanza{tsConfig = [ConfigRow [ConfigText "hi", ConfigButton _ "Go"]]}] -> pure ()
        other -> expectationFailure ("did not match expected tool stanza: " ++ show (length other) ++ " stanzas")

    it "groups consecutive tab: blocks into one set of tabs" $
      shouldParseTo [r|
@config "Configuration"
tab "First":
  a : Z <- 1
  row:
    b : Z <- 2
tab "Second":
  text "hi"
c : Z <- 3
tab "Third":
  d : Z <- 4
|] $ \case
        [ConfigStanza "Configuration"
          [ ConfigTabs [ ("First", [ConfigDecl{cdName = "a"}, ConfigRow [ConfigDecl{cdName = "b"}]])
                       , ("Second", [ConfigText "hi"]) ]
          , ConfigDecl{cdName = "c"}
          , ConfigTabs [ ("Third", [ConfigDecl{cdName = "d"}]) ]
          ]] -> pure ()
        other -> expectationFailure ("did not match expected config stanza: " ++ show (length other) ++ " stanzas")

    it "accepts tab: blocks in a tool's configuration" $
      shouldParseTo [r|
@tool "T"
tab "A":
  a : Z <- 1
tab "B":
  b : Z <- 2
on click:
  a <- b
|] $ \case
        [ToolStanza{tsConfig = [ConfigTabs [("A", [_]), ("B", [_])]]}] -> pure ()
        other -> expectationFailure ("did not match expected tool stanza: " ++ show (length other) ++ " stanzas")

    it "allows a plain declaration named `tab`" $
      shouldParseTo [r|
@config "Configuration"
tab : Z <- 5
|] $ \case
        [ConfigStanza "Configuration" [ConfigDecl{cdName = "tab"}]] -> pure ()
        other -> expectationFailure ("did not match expected config stanza: " ++ show (length other) ++ " stanzas")

    it "allows a plain declaration named `row` outside of a row: block" $
      shouldParseTo [r|
@config "Configuration"
row : Z <- 5
|] $ \case
        [ConfigStanza "Configuration" [ConfigDecl{cdName = "row"}]] -> pure ()
        other -> expectationFailure ("did not match expected config stanza: " ++ show (length other) ++ " stanzas")

    it "defaults a declaration's label to Nothing when `as` is omitted" $
      shouldParseTo [r|
@config "Configuration"
foo : R <- 1.5
|] $ \case
        [ConfigStanza "Configuration" [ConfigDecl{cdLabel = Nothing}]] -> pure ()
        other -> expectationFailure ("unexpected stanzas: " ++ show (length other))

    it "rejects attributes on a config stanza header" $
      [r|
@config "Configuration" foo="1"
|] `shouldFailWith` "takes no attributes"

    it "accepts a selection (choice) declaration on an integer type" $
      shouldParseTo [r|
@config "Configuration"
size : Z as "Size:" choice {"Small", "Medium", "Large"} <- 0
|] $ \case
        [ConfigStanza "Configuration" [ConfigDecl{cdChoices = Just ["Small", "Medium", "Large"]}]] -> pure ()
        other -> expectationFailure ("unexpected stanzas: " ++ show (length other))

    it "rejects a choice list on a non-integer declaration" $
      [r|
@config "Configuration"
size : R as "Size:" choice {"Small", "Medium", "Large"} <- 0.0
|] `shouldFailWith` "can only be used with a \8484-typed declaration"

    it "accepts a tool stanza with multiple event blocks" $
      shouldParseTo [r|
@tool "Trace" for="Mandelbrot Set" shortcut="T" drawLayer="5" refreshOnActivate="false" refreshCanUpdate="true"
on refresh:
  erase
on click-or-drag:
  pass
|] $ \case
        [ToolStanza{..}] -> do
          tsName `shouldBe` "Trace"
          tsFor `shouldBe` Just "Mandelbrot Set"
          tsShortcut `shouldBe` "T"
          tsIcon `shouldBe` Nothing
          tsDrawLayer `shouldBe` 5
          tsRefreshOnActivate `shouldBe` False
          tsRefreshCanUpdate `shouldBe` True
          length tsBlocks `shouldBe` 2
        other -> expectationFailure ("expected a single tool stanza, got " ++ show (length other) ++ " stanzas")

    it "accepts declarations before a tool's event blocks as its own config" $
      shouldParseTo [r|
@tool "Demo" shortcut="x"
r : R as "R =" <- 1
theta : R as "θ =" <- pi/4
on refresh:
  erase
|] $ \case
        [ToolStanza{tsConfig = [ConfigDecl{cdName = "r"}, ConfigDecl{cdName = "theta"}], tsBlocks}] ->
          length tsBlocks `shouldBe` 1
        other -> expectationFailure ("unexpected stanzas: " ++ show (length other))

    it "defaults a tool's own config to no declarations" $
      shouldParseTo [r|
@tool "Demo"
on refresh:
  pass
|] $ \case
        [ToolStanza{tsConfig = []}] -> pure ()
        other -> expectationFailure ("unexpected stanzas: " ++ show (length other))

    it "rejects a declaration appearing after a tool's event blocks" $
      [r|
@tool "Demo"
on refresh:
  pass
r : R <- 1
|] `shouldFailWith` "stuck"

    it "defaults drawLayer to 100, refreshOnActivate to true, refreshCanUpdate to false" $
      shouldParseTo [r|
@tool "Trace"
on refresh:
  pass
|] $ \case
        [ToolStanza{..}] -> do
          tsDrawLayer `shouldBe` 100
          tsRefreshOnActivate `shouldBe` True
          tsRefreshCanUpdate `shouldBe` False
        other -> expectationFailure ("expected a single tool stanza, got " ++ show (length other) ++ " stanzas")

    it "rejects a non-numeric drawLayer attribute" $
      [r|
@tool "Trace" drawLayer="high"
on refresh:
  pass
|] `shouldFailWith` "must be a whole number"

    it "defaults a tool's shortcut to empty and allows omitting `for`" $
      shouldParseTo [r|
@tool "Trace"
on refresh:
  pass
|] $ \case
        [ToolStanza{tsFor = Nothing, tsShortcut = ""}] -> pure ()
        other -> expectationFailure ("unexpected stanzas: " ++ show (length other))

    it "rejects an unknown tool attribute" $
      [r|
@tool "Trace" bogus="1"
on refresh:
  pass
|] `shouldFailWith` "Unknown attribute `bogus`"

    it "parses a full multi-stanza project script" $
      shouldParseTo [r|
@common
let f(z) = z^2 + C

@config "Configuration"
maxIters : Z as "Iteration limit:" <- 100

@viewer "Mandelbrot Set" var=C limit="maxIters"
z : C <- 0
iterate z -> f(z) until z escapes

@tool "Trace" for="Mandelbrot Set" shortcut="T"
on refresh:
  erase
on click-or-drag:
  pass
|] $ \stanzas -> length stanzas `shouldBe` 4

    it "no longer accepts @setup stanzas or splices" $ do
      [r|
@setup "Setup"
f : C as "f(z) =" <- 0

@viewer "V" var=z
pass
|] `shouldFailWith` "couldn't parse"
      [r|
@viewer "V" var=z
z <- «f»
|] `shouldFailWith` "couldn't parse"

    it "finds the stanza boundary right after a nested block closes \
       \(regression: Indent/Dedent used to wrongly reset line-start status)" $
      shouldParseTo [r|
@viewer "V" var=z
if z = 0:
  color <- black
else:
  color <- white

@tool "T" for="V"
on refresh:
  pass
|] $ \stanzas -> length stanzas `shouldBe` 2

    it "allows a stanza body ending in a variable declaration (the Let-slurp case)" $
      shouldParseTo [r|
@viewer "Mandelbrot Set" var=C
color : Color <- black

@tool "Trace" for="Mandelbrot Set"
on refresh:
  pass
|] $ \stanzas -> length stanzas `shouldBe` 2
