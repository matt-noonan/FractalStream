{-# language QuasiQuotes #-}
module Actor.EnsembleBuildSpec (spec) where

import Test.Hspec

import Actor.Ensemble
import Actor.Viewer.Complex (ComplexViewer(..))
import Actor.Tool (Tool(..), ToolInfo(..))
import Actor.Layout (Dimensions(..), Layout(..), UIVariable(..))
import Actor.Configuration (Configuration(..))
import Actor.Event (runParsedToolScript)
import Language.Environment (SomeContext(..), Context(EmptyContext), contextToEnv)
import Data.DynamicValue

import qualified Data.Set as Set
import Data.List (isInfixOf)
import Text.RawString.QQ

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
              Left err -> expectationFailure
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
