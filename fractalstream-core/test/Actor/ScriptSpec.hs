{-# language QuasiQuotes #-}
module Actor.ScriptSpec (spec) where

import Test.Hspec

import Language.Type

import Actor.Script
import Actor.Layout (Dimensions(..))

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
@viewer "Mandelbrot Set" var=C size="500x500"
z : C <- 0
iterate z -> z^2 + C until z escapes
|] $ \case
        [ViewerStanza{..}] -> do
          vsTitle `shouldBe` "Mandelbrot Set"
          vsVar `shouldBe` "C"
          vsSize `shouldBe` Dimensions (500, 500)
          vsCenter `shouldBe` Nothing
          vsIterLimit `shouldBe` Nothing
        other -> expectationFailure ("expected a single viewer stanza, got " ++ show (length other) ++ " stanzas")

    it "accepts optional viewer attributes" $
      shouldParseTo [r|
@viewer "Mandelbrot Set" var=C size="500x500" center="0" pixelSize="1/128" limit="maxIters" escape="maxRadius" vanish="minRadius" pixelVar=px resizable="false" position="10x20"
pass
|] $ \case
        [ViewerStanza{..}] -> do
          vsCenter `shouldBe` Just "0"
          vsPixelSize `shouldBe` Just "1/128"
          vsIterLimit `shouldBe` Just "maxIters"
          vsEscape `shouldBe` Just "maxRadius"
          vsVanish `shouldBe` Just "minRadius"
          vsPixelVar `shouldBe` Just "px"
          vsResizable `shouldBe` False
          vsPosition `shouldBe` Dimensions (10, 20)
        other -> expectationFailure ("expected a single viewer stanza, got " ++ show (length other) ++ " stanzas")

    it "defaults resizable to true, position to 100x100, and pixelVar to Nothing" $
      shouldParseTo [r|
@viewer "Mandelbrot Set" var=C size="500x500"
pass
|] $ \case
        [ViewerStanza{..}] -> do
          vsResizable `shouldBe` True
          vsPosition `shouldBe` Dimensions (100, 100)
          vsPixelVar `shouldBe` Nothing
        other -> expectationFailure ("expected a single viewer stanza, got " ++ show (length other) ++ " stanzas")

    it "rejects a non-boolean resizable attribute" $
      [r|
@viewer "Mandelbrot Set" var=C size="500x500" resizable="sideways"
pass
|] `shouldFailWith` "must be `true` or `false`"

    it "rejects a malformed position attribute" $
      [r|
@viewer "Mandelbrot Set" var=C size="500x500" position="notasize"
pass
|] `shouldFailWith` "dimension descriptor"

    it "rejects a viewer stanza missing the required `var` attribute" $
      [r|
@viewer "Mandelbrot Set" size="500x500"
pass
|] `shouldFailWith` "Missing required attribute `var`"

    it "rejects a viewer stanza with an unknown attribute" $
      [r|
@viewer "Mandelbrot Set" var=C size="500x500" bogus="1"
pass
|] `shouldFailWith` "Unknown attribute `bogus`"

    it "rejects a viewer stanza with a duplicate attribute" $
      [r|
@viewer "Mandelbrot Set" var=C var=z size="500x500"
pass
|] `shouldFailWith` "Duplicate attribute `var`"

    it "rejects a malformed size attribute" $
      [r|
@viewer "Mandelbrot Set" var=C size=notasize
pass
|] `shouldFailWith` "dimension descriptor"

    it "accepts a config stanza with declarations, text, and a button" $
      shouldParseTo [r|
@config "Configuration"
maxIters : Z as "Iteration limit:" <- 100
text "Here is some text"
button "Reset"
|] $ \case
        [ConfigStanza "Configuration"
          [ ConfigDecl{cdName = "maxIters", cdType, cdLabel = Just "Iteration limit:", cdChoices = Nothing}
          , ConfigText "Here is some text"
          , ConfigButton "Reset"
          ]] -> typeName cdType `shouldBe` "ℤ"
        other -> expectationFailure ("did not match expected config stanza: " ++ show (length other) ++ " stanzas")

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

    it "accepts text and button statements inside a row:" $
      shouldParseTo [r|
@config "Configuration"
row:
  text "hi"
  button "Go"
|] $ \case
        [ConfigStanza "Configuration" [ConfigRow [ConfigText "hi", ConfigButton "Go"]]] -> pure ()
        other -> expectationFailure ("did not match expected config stanza: " ++ show (length other) ++ " stanzas")

    it "accepts a `code «name»:` declaration, parsed with the full statement \
       \language (assignments, conditionals), not just expressions" $
      shouldParseTo [r|
@setup "Setup"
code script:
  if z escapes:
    color <- black
  else:
    color <- white
|] $ \case
        [SetupStanza "Setup" [ConfigCode{ccName = "script"}]] -> pure ()
        other -> expectationFailure ("did not match expected setup stanza: " ++ show (length other) ++ " stanzas")

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

    it "accepts a setup stanza (same body grammar as config)" $
      shouldParseTo [r|
@setup "Simple complex dynamics"
title : Text <- "Complex dynamics"
|] $ \case
        [SetupStanza "Simple complex dynamics" [ConfigDecl{cdName = "title"}]] -> pure ()
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
@setup "Simple complex dynamics"
title : Text <- "Complex dynamics"

@config "Configuration"
maxIters : Z as "Iteration limit:" <- 100

@viewer "Mandelbrot Set" var=C size="500x500" limit="maxIters"
z : C <- 0
iterate z -> z^2 + C until z escapes

@tool "Trace" for="Mandelbrot Set" shortcut="T"
on refresh:
  erase
on click-or-drag:
  pass
|] $ \stanzas -> length stanzas `shouldBe` 4

    it "accepts a `using` clause on a config declaration, making it a formula field" $
      shouldParseTo [r|
@setup "Setup"
f : C using z:C, C:C <- z^2 + C
|] $ \case
        [SetupStanza "Setup" [ConfigDecl{cdName = "f", cdUsing = Just env}]] ->
          env `shouldBe` [("z", ComplexT), ("C", ComplexT)]
        other -> expectationFailure ("did not match expected setup stanza: " ++ show (length other) ++ " stanzas")

    it "rejects both `choice` and `using` on the same declaration" $
      shouldFailWith [r|
@setup "Setup"
f : Z choice {"a", "b"} using z:C <- 0
|] "cannot use both"

    it "accepts `using` on a boolean-typed declaration (a formula stopping condition)" $
      shouldParseTo [r|
@setup "Setup"
f : Boolean using z:C <- |z| > 1
|] $ \case
        [SetupStanza "Setup" [ConfigDecl{cdName = "f", cdUsing = Just [("z", ComplexT)]}]] -> pure ()
        other -> expectationFailure ("did not match expected setup stanza: " ++ show (length other) ++ " stanzas")

    it "rejects a `using` clause with a duplicate variable name" $
      shouldFailWith [r|
@setup "Setup"
f : C using z:C, z:R <- z
|] "more than once"

    it "resolves a `«name»` splice reference across stanzas, from a `using`-clause formula" $
      shouldParseTo [r|
@setup "Setup"
f : C using z:C, C:C <- z^2 + C

@viewer "V" var=C size="100x100"
z : C <- «f»
iterate z -> «f» until z escapes
|] $ \stanzas -> length stanzas `shouldBe` 2

    it "fails to parse when a splice reference is undeclared" $
      shouldFailWith [r|
@viewer "V" var=C size="100x100"
z : C <- «nope»
pass
|] "couldn't parse"

    it "resolves a `«name»` code splice -- a whole block of statements, \
       \not just an expression -- from a `code «name»:` declaration" $
      shouldParseTo [r|
@setup "Setup"
code script:
  if |z| < 1:
    color <- black
  else:
    color <- white

@viewer "V" var=z size="100x100"
«script»
|] $ \stanzas -> length stanzas `shouldBe` 2

    it "allows stanzas in any order -- a `@viewer` before the `@setup` it splices from" $
      shouldParseTo [r|
@viewer "V" var=C size="100x100"
z : C <- «f»
pass

@setup "Setup"
f : C using z:C, C:C <- z^2 + C
|] $ \stanzas -> length stanzas `shouldBe` 2

    it "finds the stanza boundary right after a nested block closes \
       \(regression: Indent/Dedent used to wrongly reset line-start status)" $
      shouldParseTo [r|
@viewer "V" var=z size="100x100"
if z = 0:
  color <- black
else:
  color <- white

@tool "T" for="V"
on refresh:
  pass
|] $ \stanzas -> length stanzas `shouldBe` 2

    it "allows a plain declaration named `using` outside of a `using` clause" $
      shouldParseTo [r|
@config "Configuration"
using : Z <- 5
|] $ \case
        [ConfigStanza "Configuration" [ConfigDecl{cdName = "using"}]] -> pure ()
        other -> expectationFailure ("did not match expected config stanza: " ++ show (length other) ++ " stanzas")

    it "allows a stanza body ending in a variable declaration (the Let-slurp case)" $
      shouldParseTo [r|
@viewer "Mandelbrot Set" var=C size="500x500"
color : Color <- black

@tool "Trace" for="Mandelbrot Set"
on refresh:
  pass
|] $ \stanzas -> length stanzas `shouldBe` 2
