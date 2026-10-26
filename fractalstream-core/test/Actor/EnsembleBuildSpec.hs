{-# language QuasiQuotes, OverloadedStrings #-}
module Actor.EnsembleBuildSpec (spec) where

import Test.Hspec

import Actor.Ensemble
import Actor.Viewer.Complex (ComplexViewer(..))
import Actor.Tool (Tool(..), ToolInfo(..))
import Actor.Layout (Dimensions(..), Layout(..), UIVariable(..))
import Actor.Configuration (Configuration(..))
import Actor.Event (runParsedToolScript)
import Language.Environment (SomeContext(..), Context(EmptyContext), EnvironmentProxy(..), contextToEnv)
import Language.Type (TypeProxy(..))
import Language.Value (SomeValue(..))
import Language.Value.Evaluator (evaluateInContext)
import Data.DynamicValue

import qualified Data.Set as Set
import Data.List (isInfixOf)
import Text.RawString.QQ

import qualified Data.Yaml as YAML
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as Text
import qualified Data.Vector as V

build :: String -> IO (Either String Ensemble)
build src = buildEnsembleFromScript src >>= \case
  Left (_, err) -> pure (Left err)
  Right ens     -> pure (Right ens)

shouldBuild :: String -> (Ensemble -> Expectation) -> Expectation
shouldBuild src check = build src >>= \case
  Left err  -> expectationFailure ("expected success, got error: " ++ err)
  Right ens -> check ens

shouldFailWith :: String -> String -> Expectation
shouldFailWith src needle = build src >>= \case
  Left err -> err `shouldSatisfy` isInfixOf needle
  Right _  -> expectationFailure "expected a build error, but it succeeded"

spec :: Spec
spec = do

  describe "buildEnsembleFromScript" $ do

    it "builds a single viewer with no config/setup/tools" $
      shouldBuild [r|
@viewer "Mandelbrot Set" var=C size="500x500"
z : C <- 0
iterate z -> z^2 + C until z escapes
color <- if stuck then black else rainbow(iterations / 20)
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
        title <- getDynamic (cvTitle viewer)
        title `shouldBe` Right "Mandelbrot Set"
        tools <- getDynamic (cvTools viewer)
        length tools `shouldBe` 0

    it "attaches a tool to its named viewer" $
      shouldBuild [r|
@viewer "Mandelbrot Set" var=C size="500x500"
z : C <- 0
iterate z -> z^2 + C until z escapes

@tool "Trace" for="Mandelbrot Set" shortcut="T"
on refresh:
  erase
on click-or-drag:
  pass
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
        [tool] <- getDynamic (cvTools viewer)
        name <- getDynamic (tiName (toolInfo tool))
        name `shouldBe` Right "Trace"
        shortcut <- getDynamic (tiShortcut (toolInfo tool))
        shortcut `shouldBe` "T"

    it "wires the new viewer/tool attributes through to the built values" $
      shouldBuild [r|
@viewer "Mandelbrot Set" var=C size="500x500" pixelVar=px resizable="false" position="10x20"
z : C <- 0
iterate z -> z^2 + C until z escapes

@tool "Trace" for="Mandelbrot Set" drawLayer="5" refreshOnActivate="false" refreshCanUpdate="true"
on refresh:
  pass
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
        resizable <- getDynamic (cvCanResize viewer)
        resizable `shouldBe` False
        position <- getDynamic (cvPosition viewer)
        position `shouldBe` Dimensions (10, 20)
        pixelVar <- getDynamic (cvPixel viewer)
        pixelVar `shouldBe` Right (Just "px")
        [tool] <- getDynamic (cvTools viewer)
        layer <- getDynamic (toolDrawLayer tool)
        layer `shouldBe` 5
        onActivate <- getDynamic (toolRefreshOnActivate tool)
        onActivate `shouldBe` False
        canUpdate <- getDynamic (toolRefreshCanUpdate tool)
        canUpdate `shouldBe` True

    it "builds a tool's own declarations into its own configuration panel" $
      shouldBuild [r|
@viewer "V" var=z size="100x100"
pass

@tool "Demo" for="V" shortcut="x"
r : R as "R =" <- 1
theta : R as "θ =" <- pi/4
on refresh:
  r <- r + theta
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
        [tool] <- getDynamic (cvTools viewer)

        varNames <- getDynamic (toolVars tool)
        varNames `shouldBe` Set.fromList ["r", "theta"]

        mcfg <- getDynamic (toolConfig tool)
        case mcfg of
          Nothing -> expectationFailure "expected a per-tool configuration"
          Just _  -> pure ()

        -- The same (unmodified) runtime machinery a classic YAML tool's
        -- own `configuration:` key already goes through: combine the
        -- tool's own config with the (here, empty) ensemble context, and
        -- confirm the tool's own `r`/`theta` resolve inside its handler.
        script <- getDynamic (toolScript tool)
        buildToolContext (SomeContext EmptyContext) mcfg >>= \case
          Left err -> expectationFailure ("buildToolContext failed: " ++ err)
          Right (SomeContext ctx) ->
            case runParsedToolScript script (contextToEnv ctx) of
              Left (_, err) -> expectationFailure
                ("tool script failed to resolve its own config variables: " ++ err)
              Right _ -> pure ()

    it "defaults a tool with no declarations to no configuration panel" $
      shouldBuild [r|
@viewer "V" var=z size="100x100"
pass

@tool "Demo" for="V"
on refresh:
  pass
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
        [tool] <- getDynamic (cvTools viewer)
        mcfg <- getDynamic (toolConfig tool)
        case mcfg of
          Nothing -> pure ()
          Just _  -> expectationFailure "expected no per-tool configuration"
        varNames <- getDynamic (toolVars tool)
        varNames `shouldBe` Set.empty

    it "defaults a tool's target to the sole viewer when `for=` is omitted" $
      shouldBuild [r|
@viewer "Only One" var=z size="100x100"
pass

@tool "T"
on refresh:
  pass
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
        tools <- getDynamic (cvTools viewer)
        length tools `shouldBe` 1

    it "combines multiple @config stanzas into one Configuration" $
      shouldBuild [r|
@config "Dynamics"
a : Z <- 1

@config "Coloring"
b : Z <- 2

@viewer "V" var=z size="100x100"
pass
|] $ \ens -> do
        mcfg <- getDynamic (ensembleConfiguration ens)
        case mcfg of
          Nothing -> expectationFailure "expected a Configuration"
          Just _  -> pure ()

    it "builds a row: block into a Horizontal layout" $
      shouldBuild [r|
@config "Configuration"
row:
  a : Z <- 1
  b : Z <- 2
c : Z <- 3
|] $ \ens -> do
        mcfg <- getDynamic (ensembleConfiguration ens)
        case mcfg of
          Nothing -> expectationFailure "expected a Configuration"
          Just cfg -> case coContents cfg of
            Vertical panelsV -> getDynamic panelsV >>= \case
              [Panel _ bodyV] -> getDynamic bodyV >>= \case
                Vertical itemsV -> getDynamic itemsV >>= \case
                  [Horizontal rowV, _declItem] -> do
                    rowItems <- getDynamic rowV
                    length rowItems `shouldBe` 2
                  other -> expectationFailure ("unexpected panel body items: " ++ show (length other))
                _ -> expectationFailure "expected the panel body to be Vertical"
              other -> expectationFailure ("expected a single Panel, got " ++ show (length other))
            _ -> expectationFailure "expected the top-level Configuration layout to be Vertical"

    it "builds a @setup panel distinct from @config" $
      shouldBuild [r|
@setup "Setup"
title : Text <- "hello"

@viewer "V" var=z size="100x100"
pass
|] $ \ens -> do
        msetup <- getDynamic (ensembleSetup ens)
        case msetup of
          Nothing -> expectationFailure "expected a setup Configuration"
          Just _  -> pure ()
        mcfg <- getDynamic (ensembleConfiguration ens)
        case mcfg of
          Nothing -> pure ()
          Just _  -> expectationFailure "did not expect a configuration Configuration"

    it "makes a @config field's text box genuinely reactive to edits \
       \(regression: a frozen Mapped used to silently ignore them)" $
      shouldBuild [r|
@config "Configuration"
maxIters : Z <- 100
|] $ \ens -> do
        mcfg <- getDynamic (ensembleConfiguration ens)
        case mcfg of
          Nothing -> expectationFailure "expected a Configuration"
          Just cfg -> case coContents cfg of
            Vertical panelsV -> getDynamic panelsV >>= \case
              [Panel _ bodyV] -> getDynamic bodyV >>= \case
                Vertical itemsV -> getDynamic itemsV >>= \case
                  [TextBox _ UIVariable{exprValue}] -> do
                    initialValue <- getDynamic (dyn exprValue)
                    case initialValue of
                      Left err -> expectationFailure ("expected the initial value to parse, got: " ++ err)
                      Right _  -> pure ()
                    setValue (source exprValue) "not a number"
                    editedValue <- getDynamic (dyn exprValue)
                    case editedValue of
                      Right _ -> expectationFailure
                        "editing the text box didn't re-parse (or re-parsed and accepted invalid input) -- the value never changed"
                      Left _  -> pure ()
                  other -> expectationFailure ("unexpected panel body items: " ++ show (length other))
                _ -> expectationFailure "expected the panel body to be Vertical"
              other -> expectationFailure ("expected a single Panel, got " ++ show (length other))
            _ -> expectationFailure "expected the top-level Configuration layout to be Vertical"

    it "renders a small Real default as fixed-point text, not scientific notation \
       \that silently mis-parses via implicit multiplication with e \
       \(regression: show (0.05 :: Double) == \"5.0e-2\", which re-parses as 5.0 * e - 2)" $
      shouldBuild [r|
@config "Configuration"
x : R <- 0.05
|] $ \ens -> do
        mcfg <- getDynamic (ensembleConfiguration ens)
        case mcfg of
          Nothing -> expectationFailure "expected a Configuration"
          Just cfg -> case coContents cfg of
            Vertical panelsV -> getDynamic panelsV >>= \case
              [Panel _ bodyV] -> getDynamic bodyV >>= \case
                Vertical itemsV -> getDynamic itemsV >>= \case
                  [TextBox _ UIVariable{exprValue}] -> do
                    initialText <- getDynamic (source exprValue)
                    initialText `shouldSatisfy` notElem 'e'
                    getDynamic (dyn exprValue) >>= \case
                      Left err -> expectationFailure ("expected the initial value to parse, got: " ++ err)
                      Right (SomeValue EmptyEnvProxy RealType v) ->
                        evaluateInContext EmptyContext v `shouldBe` 0.05
                      Right _ -> expectationFailure "expected an empty-environment Real value"
                  other -> expectationFailure ("unexpected panel body items: " ++ show (length other))
                _ -> expectationFailure "expected the panel body to be Vertical"
              other -> expectationFailure ("expected a single Panel, got " ++ show (length other))
            _ -> expectationFailure "expected the top-level Configuration layout to be Vertical"

    it "round-trips a List-typed config field's default value \
       \(regression: toUserString strips brackets for display, so re-parsing \
       \needs parseInputValue, which re-adds them, not plain parseValue)" $
      shouldBuild [r|
@config "Configuration"
xs : List of Z <- [1, 2, 3]
|] $ \ens -> do
        mcfg <- getDynamic (ensembleConfiguration ens)
        case mcfg of
          Nothing -> expectationFailure "expected a Configuration"
          Just cfg -> case coContents cfg of
            Vertical panelsV -> getDynamic panelsV >>= \case
              [Panel _ bodyV] -> getDynamic bodyV >>= \case
                Vertical itemsV -> getDynamic itemsV >>= \case
                  [TextBox _ UIVariable{exprValue}] ->
                    getDynamic (dyn exprValue) >>= \case
                      Left err -> expectationFailure ("expected the initial value to parse, got: " ++ err)
                      Right (SomeValue EmptyEnvProxy (ListType IntegerType) v) ->
                        evaluateInContext EmptyContext v `shouldBe` [1, 2, 3]
                      Right _ -> expectationFailure "expected an empty-environment List of Integer value"
                  other -> expectationFailure ("unexpected panel body items: " ++ show (length other))
                _ -> expectationFailure "expected the panel body to be Vertical"
              other -> expectationFailure ("expected a single Panel, got " ++ show (length other))
            _ -> expectationFailure "expected the top-level Configuration layout to be Vertical"

    it "builds a `using`-clause formula field and splices it into a viewer's code" $
      shouldBuild [r|
@setup "Setup"
f : C using z:C, C:C <- z^2 + C

@viewer "Mandelbrot" var=C size="100x100"
z : C <- 0
iterate z -> «f» until z escapes
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
        title <- getDynamic (cvTitle viewer)
        title `shouldBe` Right "Mandelbrot"

    it "rejects a formula field whose expression uses a variable missing from `using`" $
      [r|
@setup "Setup"
f : C using z:C <- z^2 + C

@viewer "V" var=C size="100x100"
pass
|] `shouldFailWith` "doesn't typecheck"

    it "builds a boolean-typed `using`-clause formula and splices it as a stopping condition" $
      shouldBuild [r|
@setup "Setup"
stop : Boolean using z:C <- |z| > 1

@viewer "V" var=z size="100x100"
until «stop»:
  pass
color <- black
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
        title <- getDynamic (cvTitle viewer)
        title `shouldBe` Right "V"

    it "remembers its own source text, for a frontend's save path to write back out" $
      let src = "@viewer \"V\" var=z size=\"100x100\"\npass\n"
      in shouldBuild src $ \ens -> do
           msrc <- getDynamic (ensembleSourceScript ens)
           msrc `shouldBe` Just src

    it "encodeEnsemble writes a script-built ensemble's text back out verbatim, \
       \not re-derived through the classic codec" $
      let src = "@viewer \"V\" var=z size=\"100x100\"\npass\n"
      in shouldBuild src $ \ens -> do
           bytes <- encodeEnsemble ens
           case YAML.decodeEither' bytes of
             Left err -> expectationFailure ("re-decoding the saved bytes failed: " ++ show err)
             Right (Object o) -> case KM.lookup "script" o of
               Just (String t) -> Text.unpack t `shouldBe` src
               _ -> expectationFailure "expected a `script:` key with a string value"
             Right _ -> expectationFailure "expected a YAML object"

    it "encodeEnsemble omits scenarios: when there are none, and includes it \
       \with the scenario's name and commands when there are" $
      let src = "@viewer \"V\" var=z size=\"100x100\"\npass\n"
      in shouldBuild src $ \ens -> do
           bytesEmpty <- encodeEnsemble ens
           case YAML.decodeEither' bytesEmpty of
             Right (Object o) -> KM.member "scenarios" o `shouldBe` False
             _ -> expectationFailure "expected a YAML object"

           name <- newVariable "Scenario 1"
           commands <- newVariable "click at 0"
           setValue' (ensembleScenarios ens) [NamedScenario name commands]
           bytesWith <- encodeEnsemble ens
           case YAML.decodeEither' bytesWith of
             Right (Object o) -> case KM.lookup "scenarios" o of
               Just (Array (V.toList -> [Object s])) -> do
                 KM.lookup "name" s `shouldBe` Just (String "Scenario 1")
                 KM.lookup "commands" s `shouldBe` Just (String "click at 0")
               other -> expectationFailure ("expected a one-element scenarios list, got " ++ show other)
             _ -> expectationFailure "expected a YAML object"

    it "rejects a tool targeting an unknown viewer" $
      [r|
@viewer "A" var=z size="100x100"
pass

@tool "T" for="B"
on refresh:
  pass
|] `shouldFailWith` "targets an unknown viewer `B`"

    it "rejects a tool with no `for=` when there's more than one viewer" $
      [r|
@viewer "A" var=z size="100x100"
pass

@viewer "B" var=z size="100x100"
pass

@tool "T"
on refresh:
  pass
|] `shouldFailWith` "doesn't specify `for=`"

    it "rejects two viewers sharing a title" $
      [r|
@viewer "A" var=z size="100x100"
pass

@viewer "A" var=w size="100x100"
pass
|] `shouldFailWith` "share the title `A`"

    it "rejects a config declaration whose default doesn't match its type" $
      [r|
@config "C"
foo : Z <- "hello"
|] `shouldFailWith` "doesn't match its declared type"

    it "rejects an out-of-range choice index" $
      [r|
@config "C"
foo : Z as "Pick" choice {"a", "b"} <- 5

@viewer "V" var=z size="100x100"
pass
|] `shouldFailWith` "out of range"

    it "rejects a viewer body that references an undeclared variable" $
      [r|
@viewer "V" var=z size="100x100"
w <- w + 1
|] `shouldFailWith` "No variable named"

  describe "checkScript" $ do

    let checkFailsWith :: String -> String -> Expectation
        checkFailsWith src needle = case checkScript src of
          Left (_, err) -> err `shouldSatisfy` isInfixOf needle
          Right ()       -> expectationFailure "expected checkScript to reject this, but it accepted it"

        checkAccepts :: String -> Expectation
        checkAccepts src = case checkScript src of
          Left (_, err) -> expectationFailure ("expected checkScript to accept this, got: " ++ err)
          Right ()       -> pure ()

        -- Regression guard against 'checkScript' and 'buildEnsembleFromScript'
        -- drifting apart (see 'checkConfigDecl's doc comment): a script this
        -- asserts both accept, or both reject the same way, independently of
        -- the live-editor-specific assertions above.
        agree :: String -> Expectation
        agree src = do
          built <- build src
          case (checkScript src, built) of
            (Right (), Right _)   -> pure ()
            (Left (_, cErr), Left bErr) ->
              bErr `shouldSatisfy` isInfixOf cErr
            (Right (), Left bErr) ->
              expectationFailure ("checkScript accepted, but buildEnsembleFromScript rejected: " ++ bErr)
            (Left (_, cErr), Right _) ->
              expectationFailure ("checkScript rejected (" ++ cErr ++ "), but buildEnsembleFromScript accepted")

    it "accepts a simple, valid single-viewer script" $
      checkAccepts [r|
@viewer "Mandelbrot Set" var=C size="500x500"
z : C <- 0
iterate z -> z^2 + C until z escapes
color <- if stuck then black else rainbow(iterations / 20)
|]

    it "catches a viewer body referencing an undeclared variable, live, \
       \without building an Ensemble" $
      checkFailsWith [r|
@viewer "V" var=z size="100x100"
w <- w + 1
|] "No variable named"

    it "catches a viewer body referencing a misspelled `@config` variable" $
      checkFailsWith [r|
@config "C"
radius : R <- 2

@viewer "V" var=z size="100x100"
color <- if |z| > raduis then black else white
|] "No variable named"

    it "accepts a viewer body that correctly references a `@config` variable" $
      checkAccepts [r|
@config "C"
radius : R <- 2

@viewer "V" var=z size="100x100"
color <- if |z| > radius then black else white
|]

    it "accepts a viewer body that references a `@setup` formula splice" $
      checkAccepts [r|
@setup "Setup"
f : C using z:C, C:C <- z^2 + C

@viewer "Mandelbrot" var=C size="100x100"
z : C <- 0
iterate z -> «f» until z escapes
|]

    it "catches a `@config` declaration whose default doesn't match its type" $
      checkFailsWith [r|
@config "C"
foo : Z <- "hello"
|] "doesn't match its declared type"

    it "catches an out-of-range choice index" $
      checkFailsWith [r|
@config "C"
foo : Z as "Pick" choice {"a", "b"} <- 5

@viewer "V" var=z size="100x100"
pass
|] "out of range"

    it "catches a `using`-clause formula whose expression uses a variable \
       \missing from `using`" $
      checkFailsWith [r|
@setup "Setup"
f : C using z:C <- z^2 + C

@viewer "V" var=C size="100x100"
pass
|] "doesn't typecheck"

    it "still catches cross-stanza errors (unchanged from before this was extended)" $ do
      checkFailsWith [r|
@viewer "A" var=z size="100x100"
pass

@tool "T" for="B"
on refresh:
  pass
|] "targets an unknown viewer `B`"
      checkFailsWith [r|
@viewer "A" var=z size="100x100"
pass

@viewer "A" var=w size="100x100"
pass
|] "share the title `A`"

    it "agrees with buildEnsembleFromScript on acceptance/rejection, across a \
       \mix of valid and invalid scripts" $ do
      agree [r|
@viewer "Mandelbrot Set" var=C size="500x500"
z : C <- 0
iterate z -> z^2 + C until z escapes
|]
      agree [r|
@config "C"
radius : R <- 2

@viewer "V" var=z size="100x100"
color <- if |z| > radius then black else white
|]
      agree [r|
@viewer "V" var=z size="100x100"
w <- w + 1
|]
      agree [r|
@config "C"
foo : Z <- "hello"
|]
      agree [r|
@config "C"
foo : Z as "Pick" choice {"a", "b"} <- 5

@viewer "V" var=z size="100x100"
pass
|]
      agree [r|
@setup "Setup"
f : C using z:C, C:C <- z^2 + C

@viewer "Mandelbrot" var=C size="100x100"
z : C <- 0
iterate z -> «f» until z escapes
|]

    it "catches a tool body referencing an undeclared variable" $
      checkFailsWith [r|
@viewer "V" var=z size="100x100"
pass

@tool "T" for="V"
on click:
  draw point at badvar
|] "No variable named"

    it "accepts a tool body that correctly references a `@config` variable" $
      checkAccepts [r|
@config "C"
center : C <- 0

@viewer "V" var=z size="100x100"
pass

@tool "T" for="V"
on click:
  draw point at center
|]

    it "accepts a tool body that references the tool's own declaration" $
      checkAccepts [r|
@viewer "V" var=z size="100x100"
pass

@tool "T" for="V"
center : C <- 0
on click:
  draw point at center
|]

    it "catches a tool's own declaration sharing a name with the ensemble's \
       \`@config`, the same as `buildToolContext`'s own combining would at \
       \runtime" $
      checkFailsWith [r|
@config "C"
center : C <- 0

@viewer "V" var=z size="100x100"
pass

@tool "T" for="V"
center : C <- 1
on click:
  draw point at center
|] "is defined twice"

    it "catches a tool error that buildEnsembleFromScript itself doesn't \
       \catch eagerly -- new coverage, not something the drift guard above \
       \expects parity on (see checkScript's own doc comment on this gap)" $ do
      let src = [r|
@viewer "V" var=z size="100x100"
pass

@tool "T" for="V"
on click:
  draw point at badvar
|]
      checkFailsWith src "No variable named"
      build src >>= \case
        Left err -> expectationFailure ("expected buildEnsembleFromScript to still accept this (the gap this test documents), got: " ++ err)
        Right _  -> pure ()
