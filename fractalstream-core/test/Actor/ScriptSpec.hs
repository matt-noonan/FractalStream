{-# language QuasiQuotes #-}
module Actor.ScriptSpec (spec) where

import Test.Hspec

import Language.Type

import Actor.Script
import Actor.Event (Coordinate(..), ViewKind(..))


import Data.List (isInfixOf)
import Control.Monad (forM_)
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
@viewer "Mandelbrot Set" over C
z : C := 0
iterate z -> z^2 + C until z escapes
|] $ \case
        [ViewerStanza{..}] -> do
          vsTitle `shouldBe` "Mandelbrot Set"
          vsCoord `shouldBe` ComplexCoordinate "C"
          vsCenter `shouldBe` Nothing
          -- Omitted, so defaults to a reference to the automatically
          -- provided `max_iters` (see 'validateViewerHeader') --
          -- not `Nothing` anymore.
          vsIterLimit `shouldBe` Just "max_iters"
        other -> expectationFailure ("expected a single viewer stanza, got " ++ show (length other) ++ " stanzas")

    it "accepts optional viewer attributes" $
      shouldParseTo [r|
@viewer "Mandelbrot Set" over C
setup:
  center: 0
  pixel size: 1/128
  iteration limit: maxIters
  escape radius: maxRadius
  vanishing radius: minRadius
pass
|] $ \case
        [ViewerStanza{..}] -> do
          vsCenter `shouldBe` Just "0"
          vsPixelSize `shouldBe` Just "1/128"
          vsIterLimit `shouldBe` Just "maxIters"
          vsEscape `shouldBe` Just "maxRadius"
          vsVanish `shouldBe` Just "minRadius"
          vsPixelVar `shouldBe` Just "pixel_size"
        other -> expectationFailure ("expected a single viewer stanza, got " ++ show (length other) ++ " stanzas")

    it "defaults pixelVar to a reference to the automatically-provided \
       \`pixel_size`" $
      shouldParseTo [r|
@viewer "Mandelbrot Set" over C
pass
|] $ \case
        [ViewerStanza{..}] -> do
          vsPixelVar `shouldBe` Just "pixel_size"
        other -> expectationFailure ("expected a single viewer stanza, got " ++ show (length other) ++ " stanzas")

    it "rejects the window-geometry attributes `size`, `position`, and \
       \`resizable`, pointing at where that state is saved now" $ do
      [r|
@viewer "Mandelbrot Set" over C size=500x500
pass
|] `shouldFailWith` "it's saved with the project"
      [r|
@viewer "Mandelbrot Set" over C position="10x20"
pass
|] `shouldFailWith` "it's saved with the project"
      [r|
@viewer "Mandelbrot Set" over C resizable=false
pass
|] `shouldFailWith` "it's saved with the project"

    it "defaults a viewer's `var` to `z` when the attribute is omitted" $
      shouldParseTo [r|
@viewer "Mandelbrot Set"
pass
|] $ \case
        [ViewerStanza{..}] -> do
          vsCoord `shouldBe` ComplexCoordinate "z"
        other -> expectationFailure ("expected a single viewer stanza, got " ++ show (length other) ++ " stanzas")

    it "accepts unquoted attribute values: a whole number (negative \
       \too), a whole-number fraction, and a boolean" $
      shouldParseTo [r|
@viewer "Mandelbrot Set" over C
setup:
  center: -2
  pixel size: 1/128
pass


@tool "Demo"
setup:
  draw layer: -1
  refresh on activate: false
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
@viewer "Mandelbrot Set"
setup:
  center: -0.5
  pixel size: 0.0001
pass
|] $ \case
        [ViewerStanza{..}] -> do
          vsCenter `shouldBe` Just "-0.5"
          vsPixelSize `shouldBe` Just "0.0001"
        other -> expectationFailure ("expected a single viewer stanza, got " ++ show (length other) ++ " stanzas")

    it "rejects a viewer stanza with an unknown attribute" $
      [r|
@viewer "Mandelbrot Set" over C bogus="1"
pass
|] `shouldFailWith` "`bogus` isn't anything a header says"

    it "rejects a viewer stanza that says something twice" $ do
      [r|
@viewer "Mandelbrot Set" over z over w
pass
|] `shouldFailWith` "says `over` twice"
      [r|
@viewer "Mandelbrot Set"
setup:
  center: 0
  center: 1
pass
|] `shouldFailWith` "sets `center` twice"



    it "accepts a config stanza with declarations and text" $
      shouldParseTo [r|
@config "Configuration"
maxIters : Z as "Iteration limit:" := 100
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
|] `shouldFailWith` "Buttons can only"
      [r|
@config "Configuration"
tab "A":
  row:
    button "Reset"
|] `shouldFailWith` "Buttons can only"

    it "decodes escape sequences in text" $
      shouldParseTo "@config \"C\"\ntext \"say \\\"hi\\\" \\\\ bye\"\n" $ \case
        [ConfigStanza "C" [ConfigText t]] -> t `shouldBe` "say \"hi\" \\ bye"
        other -> expectationFailure ("unexpected stanzas: " ++ show (length other))

    it "accepts a row: block grouping declarations side by side" $
      shouldParseTo [r|
@config "Configuration"
row:
  a : Z := 1
  b : Z := 2
c : Z := 3
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
  a : Z := 1
  row:
    b : Z := 2
tab "Second":
  text "hi"
c : Z := 3
tab "Third":
  d : Z := 4
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
  a : Z := 1
tab "B":
  b : Z := 2
on click:
  a <- b
|] $ \case
        [ToolStanza{tsConfig = [ConfigTabs [("A", [_]), ("B", [_])]]}] -> pure ()
        other -> expectationFailure ("did not match expected tool stanza: " ++ show (length other) ++ " stanzas")

    it "allows a plain declaration named `tab`" $
      shouldParseTo [r|
@config "Configuration"
tab : Z := 5
|] $ \case
        [ConfigStanza "Configuration" [ConfigDecl{cdName = "tab"}]] -> pure ()
        other -> expectationFailure ("did not match expected config stanza: " ++ show (length other) ++ " stanzas")

    it "allows a plain declaration named `row` outside of a row: block" $
      shouldParseTo [r|
@config "Configuration"
row : Z := 5
|] $ \case
        [ConfigStanza "Configuration" [ConfigDecl{cdName = "row"}]] -> pure ()
        other -> expectationFailure ("did not match expected config stanza: " ++ show (length other) ++ " stanzas")

    it "defaults a declaration's label to Nothing when `as` is omitted" $
      shouldParseTo [r|
@config "Configuration"
foo : R := 1.5
|] $ \case
        [ConfigStanza "Configuration" [ConfigDecl{cdLabel = Nothing}]] -> pure ()
        other -> expectationFailure ("unexpected stanzas: " ++ show (length other))

    it "reads a header's phrases in any order, and its setup: block" $ do
      shouldParseTo [r|
@config "P"
τ : C := i

@viewer "Torus" modulo the lattice (1, τ) centered at -0.5 over w
setup:
  pixel size: 1/150
  show if: τ ≠ 0 and true
  colorizer: "Smooth"
  coloring:
    color_speed <- 3
pass
|] $ \case
        [_, ViewerStanza{..}] -> do
          vsCoord `shouldBe` ComplexCoordinate "w"
          vsLattice `shouldBe` Just "(1, τ)"
          vsLatticeCenter `shouldBe` Just "-0.5"
          vsPixelSize `shouldBe` Just "1/150"
          vsShowIf `shouldBe` Just "τ ≠ 0 and true"
          vsColorizer `shouldBe` Just "Smooth"
          map czsName vsColoring `shouldBe` ["color_speed"]
        other -> expectationFailure ("expected two stanzas, got " ++ show (length other))
      shouldParseTo "@viewer \"J\" on the half-plane modulo the modular group over z\npass\n" $ \case
        [ViewerStanza{..}] -> (vsView, vsGroup) `shouldBe` (HalfPlaneView, Just "modular")
        other -> expectationFailure ("expected one stanza, got " ++ show (length other))
      shouldParseTo "@viewer \"R\" over (a, b)\npass\n\n@tool \"T\" with shortcut t for \"R\"\nsetup:\n  draw layer: 3\non click:\n  pass\n" $ \case
        [ViewerStanza{..}, ToolStanza{..}] -> do
          vsCoord `shouldBe` RealCoordinates "a" "b"
          (tsFor, tsShortcut, tsDrawLayer) `shouldBe` (Just "R", "t", 3)
        other -> expectationFailure ("expected two stanzas, got " ++ show (length other))

    it "says what's wrong with a header, and what replaces an old attribute" $ do
      "@viewer \"V\" var=C\npass\n" `shouldFailWith` "`var=C` is now `over C`"
      "@viewer \"V\" pixelSize=1/100\npass\n" `shouldFailWith` "`pixel size: 1/100`"
      "@tool \"T\" for=\"V\"\non click:\n  pass\n" `shouldFailWith` "`for \"V\"`"
      "@viewer \"V\" modulo the lattice τ + 1\npass\n" `shouldFailWith` "`modulo the lattice (τ + 1)`"
      "@viewer \"V\" with shortcut t\npass\n" `shouldFailWith` "doesn't say `with shortcut`"
      "@viewer \"V\"\nsetup:\n  zoom: 2\npass\n" `shouldFailWith` "no `zoom` setting"
      "@viewer \"V\"\ncoloring:\n  x <- 1\npass\n" `shouldFailWith` "inside its `setup:` block"

    it "rejects attributes on a config stanza header" $
      [r|
@config "Configuration" foo="1"
|] `shouldFailWith` "says nothing but its title"

    it "accepts a selection (choice) declaration on an integer type" $
      shouldParseTo [r|
@config "Configuration"
size : Z as "Size:" choice {"Small", "Medium", "Large"} := 0
|] $ \case
        [ConfigStanza "Configuration" [ConfigDecl{cdChoices = Just ["Small", "Medium", "Large"]}]] -> pure ()
        other -> expectationFailure ("unexpected stanzas: " ++ show (length other))

    it "rejects a choice list on a non-integer declaration" $
      [r|
@config "Configuration"
size : R as "Size:" choice {"Small", "Medium", "Large"} := 0.0
|] `shouldFailWith` "can only be used with a \8484-typed declaration"

    it "accepts a field holding a point of the Riemann sphere, or a list or tuple of them" $ do
      shouldParseTo [r|
@config "Configuration"
c : Cinf := ∞
p : P1 := [1 : 0]
|] $ \case
        [ConfigStanza "Configuration" [ConfigDecl{cdPoint = Just ExtComplexT}, ConfigDecl{cdPoint = Just ProjectiveT}]] -> pure ()
        other -> expectationFailure ("unexpected stanzas: " ++ show (length other))
      shouldParseTo [r|
@config "Configuration"
cs : List of Cinf := [∞, 1]
q : Cinf x R := (∞, 2.0)
|] $ \case
        [ConfigStanza "Configuration" [ ConfigDecl{cdPoint = Just (ListT ExtComplexT)}
                                      , ConfigDecl{cdPoint = Just (Pair ExtComplexT RealT)} ]] -> pure ()
        other -> expectationFailure ("unexpected stanzas: " ++ show (length other))

    it "accepts a tool stanza with multiple event blocks" $
      shouldParseTo [r|
@tool "Trace" for "Mandelbrot Set" with shortcut T
setup:
  draw layer: 5
  refresh on activate: false
  refresh can update: true
on refresh:
  erase
on click:
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
          -- (refresh, click, and click handling drags: see 'Actor.Event.mergeToolBlocks')
          length tsBlocks `shouldBe` 3
        other -> expectationFailure ("expected a single tool stanza, got " ++ show (length other) ++ " stanzas")

    it "accepts declarations before a tool's event blocks as its own config" $
      shouldParseTo [r|
@tool "Demo" with shortcut x
r : R as "R =" := 1
theta : R as "θ =" := pi/4
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
r : R := 1
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
@tool "Trace"
setup:
  draw layer: high
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
|] `shouldFailWith` "`bogus` isn't anything a header says"

    it "parses `@tool overlay` as a viewer's overlay, on the lowest draw layer" $
      shouldParseTo [r|
@tool overlay for "V"
on refresh:
  erase
|] $ \case
        [ToolStanza{tsOverlay = True, tsFor = Just "V", tsDrawLayer = layer, tsShortcut = ""}] ->
          layer `shouldBe` overlayDrawLayer
        other -> expectationFailure ("unexpected stanzas: " ++ show (length other))

    it "parses a tool named \"overlay\" (quoted) as an ordinary tool" $
      shouldParseTo [r|
@tool "overlay"
on click:
  pass
|] $ \case
        [ToolStanza{tsOverlay = False, tsName = "overlay"}] -> pure ()
        other -> expectationFailure ("unexpected stanzas: " ++ show (length other))

    it "rejects anything but `for` in an overlay's header" $
      [r|
@tool overlay for "V" with shortcut o
on refresh:
  pass
|] `shouldFailWith` "can only say `for \"viewer\"`, not `with shortcut`"

    it "rejects an overlay's own declarations" $
      [r|
@tool overlay
r : R := 1
on refresh:
  pass
|] `shouldFailWith` "An overlay can't declare settings of its own"

    it "rejects an overlay's blocks other than `on refresh:`" $
      [r|
@tool overlay
on refresh:
  pass
on click:
  pass
|] `shouldFailWith` "it can't have an `on click:` block"

    it "rejects an overlay without an `on refresh:` block" $
      [r|
@tool overlay

@viewer "V"
pass
|] `shouldFailWith` "An overlay needs an `on refresh:` block"

    it "parses a full multi-stanza project script" $
      shouldParseTo [r|
@common
f(z) := z^2 + C

@config "Configuration"
maxIters : Z as "Iteration limit:" := 100

@viewer "Mandelbrot Set" over C
setup:
  iteration limit: maxIters
z : C := 0
iterate z -> f(z) until z escapes

@tool "Trace" for "Mandelbrot Set" with shortcut T
on refresh:
  erase
on click:
  pass
|] $ \stanzas -> length stanzas `shouldBe` 4

    it "no longer accepts @setup stanzas or splices" $ do
      [r|
@setup "Setup"
f : C as "f(z) =" := 0

@viewer "V" over z
pass
|] `shouldFailWith` "couldn't parse"
      [r|
@viewer "V" over z
z <- «f»
|] `shouldFailWith` "couldn't parse"

    it "finds the stanza boundary right after a nested block closes \
       \(regression: Indent/Dedent used to wrongly reset line-start status)" $
      shouldParseTo [r|
@viewer "V" over z
if z = 0:
  color <- black
else:
  color <- white

@tool "T" for "V"
on refresh:
  pass
|] $ \stanzas -> length stanzas `shouldBe` 2

    it "allows a stanza body ending in a variable declaration (the Let-slurp case)" $
      shouldParseTo [r|
@viewer "Mandelbrot Set" over C
color : Color := black

@tool "Trace" for "Mandelbrot Set"
on refresh:
  pass
|] $ \stanzas -> length stanzas `shouldBe` 2

  describe "fields named with built-in names" $ do
    it "are the script's, in @config stanzas, their tabs and rows, and tools" $ do
      forM_ [ "@config \"P\"\nred : Color := blue\n"
            , "@config \"P\"\nbar : C := 0\n"
            , "@config \"P\"\ntab \"T\":\n  sin : R := 1\n"
            , "@config \"P\"\nrow:\n  x : R := 1\n  re : R := 2\n"
            , "@viewer \"V\"\ncolor <- black\n\n@tool \"T\"\nsin : R := 1\non click:\n  pass\n" ] $ \src ->
        shouldParseTo src (const (pure ()))
    it "but not keywords" $ do
      "@config \"P\"\nlength : Z := 3\n" `shouldFailWith` "`length` is a built-in function, so it can't also name a variable"
      "@config \"P\"\nrow:\n  iterations : Z := 3\n" `shouldFailWith` "`iterations` is a built-in value"
