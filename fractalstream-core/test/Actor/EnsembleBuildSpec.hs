{-# language QuasiQuotes, OverloadedStrings #-}
module Actor.EnsembleBuildSpec (spec) where

import Test.Hspec

import Actor.Ensemble
import Actor.Viewer (SomeViewerWithContext(..))
import Actor.Viewer.Complex (ComplexViewer(..))
import Actor.Tool (Tool(..), ToolInfo(..))
import Actor.Layout (Dimensions(..), Layout(..), Label(..), UIVariable(..), UIScript(..), CodeString(..))
import Actor.Configuration (Configuration(..))
import Actor.Event (runParsedToolScript)
import Language.Environment (SomeContext(..), SomeContext'(..), Context(EmptyContext), EnvironmentProxy(..), contextToEnv)
import Language.Type (TypeProxy(..))
import Language.Value (SomeValue(..))
import Language.Value.Evaluator (evaluateInContext)
import Language.Code (pprintCode)
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
        -- tool's own config with the *real* ensemble context (not an
        -- empty stand-in -- every ensemble now always has at least
        -- `max_iters`/`escape_radius`/`vanishing_radius` bound, via the
        -- automatically-provided "Iteration settings" group, which a
        -- tool's own bookkeeping always needs to resolve regardless of
        -- whether its own handler body references them), and confirm
        -- the tool's own `r`/`theta` resolve inside its handler too.
        script <- getDynamic (toolScript tool)
        Just ensembleCfg <- getDynamic (ensembleConfiguration ens)
        SomeContext' eEnsembleArgs <- layoutToArgs (coContents ensembleCfg)
        case eEnsembleArgs of
          Left err -> expectationFailure ("layoutToArgs failed: " ++ err)
          Right ensembleArgs ->
            buildToolContext ensembleArgs mcfg >>= \case
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
          -- Two panels, not one: every ensemble now also gets an
          -- automatically-provided "Iteration settings" panel
          -- (max_iters/escape_radius/vanishing_radius) appended after whatever
          -- the script's own @config declares.
          Just cfg -> case coContents cfg of
            Vertical panelsV -> getDynamic panelsV >>= \case
              [Panel _ bodyV, _implicitPanel] -> getDynamic bodyV >>= \case
                Vertical itemsV -> getDynamic itemsV >>= \case
                  [Horizontal rowV, _declItem] -> do
                    rowItems <- getDynamic rowV
                    length rowItems `shouldBe` 2
                  other -> expectationFailure ("unexpected panel body items: " ++ show (length other))
                _ -> expectationFailure "expected the panel body to be Vertical"
              other -> expectationFailure ("expected two Panels (the script's own, plus the implicit one), got " ++ show (length other))
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
        -- No explicit @config stanza at all, but ensembleConfiguration
        -- is never Nothing anymore: every ensemble always gets at
        -- least the automatically-provided "Iteration settings" panel
        -- (max_iters/escape_radius/vanishing_radius), distinct from @setup's
        -- own Configuration either way.
        mcfg <- getDynamic (ensembleConfiguration ens)
        case mcfg of
          Nothing  -> expectationFailure "expected the implicit \"Iteration settings\" Configuration"
          Just cfg -> case coContents cfg of
            Vertical panelsV -> getDynamic panelsV >>= \ps -> length ps `shouldBe` 1
            _ -> expectationFailure "expected the top-level Configuration layout to be Vertical"

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
              [Panel _ bodyV, _implicitPanel] -> getDynamic bodyV >>= \case
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
              other -> expectationFailure ("expected two Panels (the script's own, plus the implicit one), got " ++ show (length other))
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
              [Panel _ bodyV, _implicitPanel] -> getDynamic bodyV >>= \case
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
              other -> expectationFailure ("expected two Panels (the script's own, plus the implicit one), got " ++ show (length other))
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
              [Panel _ bodyV, _implicitPanel] -> getDynamic bodyV >>= \case
                Vertical itemsV -> getDynamic itemsV >>= \case
                  [TextBox _ UIVariable{exprValue}] ->
                    getDynamic (dyn exprValue) >>= \case
                      Left err -> expectationFailure ("expected the initial value to parse, got: " ++ err)
                      Right (SomeValue EmptyEnvProxy (ListType IntegerType) v) ->
                        evaluateInContext EmptyContext v `shouldBe` [1, 2, 3]
                      Right _ -> expectationFailure "expected an empty-environment List of Integer value"
                  other -> expectationFailure ("unexpected panel body items: " ++ show (length other))
                _ -> expectationFailure "expected the panel body to be Vertical"
              other -> expectationFailure ("expected two Panels (the script's own, plus the implicit one), got " ++ show (length other))
            _ -> expectationFailure "expected the top-level Configuration layout to be Vertical"

    it "automatically provides max_iters/escape_radius/vanishing_radius in an \
       \\"Iteration settings\" panel, with the right types and defaults, \
       \even when the script declares no @config at all" $
      shouldBuild [r|
@viewer "V" var=z size="100x100"
pass
|] $ \ens -> do
        Just cfg <- getDynamic (ensembleConfiguration ens)
        case coContents cfg of
          Vertical panelsV -> getDynamic panelsV >>= \case
            [Panel titleV bodyV] -> do
              Label title <- getDynamic titleV
              title `shouldBe` "Iteration settings"
              getDynamic bodyV >>= \case
                Vertical itemsV -> getDynamic itemsV >>= \items -> do
                  length items `shouldBe` 3
                  let uivars = [ uv | TextBox _ uv <- items ]
                  length uivars `shouldBe` 3
                  values <- mapM (getDynamic . dyn . exprValue) uivars
                  case values of
                    [ Right (SomeValue EmptyEnvProxy IntegerType i)
                      , Right (SomeValue EmptyEnvProxy RealType r1)
                      , Right (SomeValue EmptyEnvProxy RealType r2) ] -> do
                      evaluateInContext EmptyContext i  `shouldBe` 100
                      evaluateInContext EmptyContext r1 `shouldBe` 10
                      evaluateInContext EmptyContext r2 `shouldBe` 0.0001
                    _ -> expectationFailure ("expected Z, R, R defaults 100/10/0.0001, got: " ++ show (length values) ++ " values")
                _ -> expectationFailure "expected the panel body to be Vertical"
            other -> expectationFailure ("expected a single implicit Panel, got " ++ show (length other))
          _ -> expectationFailure "expected the top-level Configuration layout to be Vertical"

    it "lets an explicit `limit=`/`escape=`/`vanish=`/`pixelVar=` override \
       \still point at the script's own custom-named variable instead of \
       \the automatic max_iters/escape_radius/vanishing_radius/pixel_size" $
      shouldBuild [r|
@config "Configuration"
myLimit : Z <- 50
myEscape : R <- 5
myVanish : R <- 0.01

@viewer "V" var=z size="100x100" limit="myLimit" escape="myEscape" vanish="myVanish" pixelVar="myPixel"
color <- if myPixel > 0 then black else white
|] $ \_ -> pure ()

    it "a user-declared `max_iters` collides cleanly with the automatic \
       \one, same as any other duplicate config name" $
      [r|
@config "Configuration"
max_iters : Z <- 50

@viewer "V" var=z size="100x100"
pass
|] `shouldFailWith` "defined twice"

    it "a nullary event handler (`on refresh:`) can now reference the \
       \automatically-provided `pixel_size` (regression: it previously had \
       \no way to name the per-pixel scale at all for this handler kind)" $
      shouldBuild [r|
@viewer "V" var=z size="100x100"
color <- black

@tool "T" shortcut="t"
on refresh:
  scale : R <- pixel_size * 2
|] $ \_ -> pure ()

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

    it "shows a `using`-clause formula field's own source text in its Setup \
       \panel, not a placeholder (regression: used to show `name (defined \
       \in script)` -- constParsed, unlike reactiveFormulaValue, never \
       \updates on an edit either), and re-parses it reactively on edit" $
      shouldBuild [r|
@setup "Setup"
f : C using z:C, C:C <- z^2 + C

@viewer "Mandelbrot" var=C size="100x100"
z : C <- 0
iterate z -> «f» until z escapes
|] $ \ens -> do
        Just setupCfg <- getDynamic (ensembleSetup ens)
        case coContents setupCfg of
          Vertical panelsV -> getDynamic panelsV >>= \case
            [Panel _ bodyV] -> getDynamic bodyV >>= \case
              Vertical itemsV -> getDynamic itemsV >>= \case
                [TextBox _ UIVariable{exprValue}] -> do
                  initialText <- getDynamic (source exprValue)
                  initialText `shouldBe` "z^2 + C"
                  getDynamic (dyn exprValue) >>= \case
                    Left err -> expectationFailure ("expected the initial formula to parse, got: " ++ err)
                    Right _  -> pure ()
                  setValue (source exprValue) "z^3"
                  getDynamic (dyn exprValue) >>= \case
                    Left err -> expectationFailure ("expected the edited formula to parse, got: " ++ err)
                    Right _  -> pure ()
                  setValue (source exprValue) "z +"
                  getDynamic (dyn exprValue) >>= \case
                    Right _ -> expectationFailure "expected the malformed edit to be rejected"
                    Left _  -> pure ()
                other -> expectationFailure ("expected a single formula TextBox, got " ++ show (length other))
              _ -> expectationFailure "expected the panel body to be Vertical"
            other -> expectationFailure ("expected a single Panel, got " ++ show (length other))
          _ -> expectationFailure "expected the Setup panel's top-level layout to be Vertical"

    it "rebuildWithCurrentSetup actually splices an edited `using`-clause \
       \formula field's new text into the viewer it's spliced into (regression: \
       \the Setup wizard's \"Go!\" button used to just reveal the viewer(s) \
       \already built from the original script text, with rerunSetup wired \
       \to a no-op, so an edit never reached anything downstream)" $
      shouldBuild [r|
@setup "Setup"
f : C using z:C, C:C <- z^2 + C

@viewer "Mandelbrot" var=C size="100x100"
z : C <- 0
iterate z -> «f» until z escapes
|] $ \ens -> do
        Just setupCfg <- getDynamic (ensembleSetup ens)
        case coContents setupCfg of
          Vertical panelsV -> getDynamic panelsV >>= \case
            [Panel _ bodyV] -> getDynamic bodyV >>= \case
              Vertical itemsV -> getDynamic itemsV >>= \case
                [TextBox _ UIVariable{exprValue}] -> do
                  setValue (source exprValue) "z^3 + 42"
                  rebuildWithCurrentSetup ens >>= \case
                    Left err -> expectationFailure ("expected the rebuild to succeed, got: " ++ show err)
                    Right _  -> pure ()
                  [viewer] <- getDynamic (ensembleViewers ens)
                  getDynamic (cvCode viewer) >>= \case
                    Left (_, err) -> expectationFailure ("expected the rebuilt viewer's code to build, got: " ++ err)
                    Right (SomeViewerWithContext _ _ code) ->
                      pprintCode code `shouldSatisfy` isInfixOf "z ^ 3"
                other -> expectationFailure ("expected a single formula TextBox, got " ++ show (length other))
              _ -> expectationFailure "expected the panel body to be Vertical"
            other -> expectationFailure ("expected a single Panel, got " ++ show (length other))
          _ -> expectationFailure "expected the Setup panel's top-level layout to be Vertical"

    it "rebuildWithCurrentSetup rejects a malformed edit instead of building \
       \a viewer with it" $
      shouldBuild [r|
@setup "Setup"
f : C using z:C, C:C <- z^2 + C

@viewer "Mandelbrot" var=C size="100x100"
z : C <- 0
iterate z -> «f» until z escapes
|] $ \ens -> do
        Just setupCfg <- getDynamic (ensembleSetup ens)
        case coContents setupCfg of
          Vertical panelsV -> getDynamic panelsV >>= \case
            [Panel _ bodyV] -> getDynamic bodyV >>= \case
              Vertical itemsV -> getDynamic itemsV >>= \case
                [TextBox _ UIVariable{exprValue}] -> do
                  setValue (source exprValue) "z +"
                  rebuildWithCurrentSetup ens >>= \case
                    Right _   -> expectationFailure "expected the rebuild to be rejected"
                    Left _    -> pure ()
                other -> expectationFailure ("expected a single formula TextBox, got " ++ show (length other))
              _ -> expectationFailure "expected the panel body to be Vertical"
            other -> expectationFailure ("expected a single Panel, got " ++ show (length other))
          _ -> expectationFailure "expected the Setup panel's top-level layout to be Vertical"

    it "rebuildWithCurrentSetup succeeds, even with no edits at all, when \
       \@setup has a `code «name»:` splice (regression: a code splice has no \
       \Setup-panel widget -- its own Layout is an invisible empty Vertical \
       \-- so layoutToSplices's recomputed Splices always had an empty \
       \codeSplices, making «name» unrecognizable on re-parse and crashing \
       \\"Go!\" outright, independently of any edit)" $
      shouldBuild [r|
@setup "Setup"
code script:
  iterate z -> z^2 + i until z escapes
  color <- if stuck then black else white

@viewer "V" var=z size="100x100"
«script»
|] $ \ens ->
        rebuildWithCurrentSetup ens >>= \case
          Left (_, err) -> expectationFailure ("expected the rebuild to succeed, got: " ++ err)
          Right _        -> pure ()

    it "a `code «name» using <env>:` declaration gets a real ScriptBox \
       \preview/edit widget, showing the block's own raw text and \
       \type-checking it (falling back to offering the bookkeeping names \
       \`escapes`/`stuck` resolve to, same as a formula field's `using` \
       \fallback, since the declared env alone doesn't have them)" $
      shouldBuild [r|
@setup "Setup"
code script using z:C:
  iterate z -> z^2 + i until z escapes
  color <- if stuck then black else white

@viewer "V" var=z size="100x100"
«script»
|] $ \ens -> do
        Just setupCfg <- getDynamic (ensembleSetup ens)
        case coContents setupCfg of
          Vertical panelsV -> getDynamic panelsV >>= \case
            [Panel _ bodyV] -> getDynamic bodyV >>= \case
              Vertical itemsV -> getDynamic itemsV >>= \case
                [ScriptBox UIScript{scriptCode}] -> do
                  initialText <- getDynamic (source scriptCode)
                  initialText `shouldBe`
                    CodeString "iterate z -> z^2 + i until z escapes\ncolor <- if stuck then black else white"
                  getDynamic (dyn scriptCode) >>= \case
                    Left err -> expectationFailure ("expected the block to typecheck, got: " ++ show err)
                    Right _  -> pure ()
                other -> expectationFailure ("expected a single ScriptBox, got " ++ show (length other))
              _ -> expectationFailure "expected the panel body to be Vertical"
            other -> expectationFailure ("expected a single Panel, got " ++ show (length other))
          _ -> expectationFailure "expected the Setup panel's top-level layout to be Vertical"

    it "a `code «name»:` declaration with no `using` clause still gets the \
       \old invisible, empty placeholder (unchanged)" $
      shouldBuild [r|
@setup "Setup"
code script:
  color <- black

@viewer "V" var=z size="100x100"
«script»
|] $ \ens -> do
        Just setupCfg <- getDynamic (ensembleSetup ens)
        case coContents setupCfg of
          Vertical panelsV -> getDynamic panelsV >>= \case
            [Panel _ bodyV] -> getDynamic bodyV >>= \case
              Vertical itemsV -> getDynamic itemsV >>= \case
                [Vertical emptyV] -> getDynamic emptyV >>= \ls ->
                  length ls `shouldBe` 0
                other -> expectationFailure ("expected a single empty Vertical, got " ++ show (length other))
              _ -> expectationFailure "expected the panel body to be Vertical"
            other -> expectationFailure ("expected a single Panel, got " ++ show (length other))
          _ -> expectationFailure "expected the Setup panel's top-level layout to be Vertical"

    it "editing a `code «name» using <env>:` declaration's ScriptBox and \
       \calling rebuildWithCurrentSetup actually splices the edit into the \
       \viewer it's spliced into (same as an edited `using`-clause formula \
       \field already does)" $
      shouldBuild [r|
@setup "Setup"
code script using z:C:
  iterate z -> z^2 + i until z escapes
  color <- if stuck then black else white

@viewer "V" var=z size="100x100"
«script»
|] $ \ens -> do
        Just setupCfg <- getDynamic (ensembleSetup ens)
        case coContents setupCfg of
          Vertical panelsV -> getDynamic panelsV >>= \case
            [Panel _ bodyV] -> getDynamic bodyV >>= \case
              Vertical itemsV -> getDynamic itemsV >>= \case
                [ScriptBox UIScript{scriptCode}] -> do
                  setValue (source scriptCode) $ CodeString
                    "iterate z -> z^2 + 99 until z escapes\ncolor <- if stuck then black else white"
                  rebuildWithCurrentSetup ens >>= \case
                    Left (_, err) -> expectationFailure ("expected the rebuild to succeed, got: " ++ err)
                    Right _       -> pure ()
                  [viewer] <- getDynamic (ensembleViewers ens)
                  getDynamic (cvCode viewer) >>= \case
                    Left (_, err) -> expectationFailure ("expected the rebuilt viewer's code to build, got: " ++ err)
                    Right (SomeViewerWithContext _ _ code) ->
                      pprintCode code `shouldSatisfy` isInfixOf "99"
                other -> expectationFailure ("expected a single ScriptBox, got " ++ show (length other))
              _ -> expectationFailure "expected the panel body to be Vertical"
            other -> expectationFailure ("expected a single Panel, got " ++ show (length other))
          _ -> expectationFailure "expected the Setup panel's top-level layout to be Vertical"

    it "the actual splice substitution stays raw/untyped: an edit that \
       \fails the ScriptBox's own preview typecheck (because the declared \
       \`using` env doesn't happen to cover what's needed) can still \
       \rebuild fine, as long as it typechecks against the real viewer \
       \it's spliced into" $
      shouldBuild [r|
@setup "Setup"
code script using dummy:Z:
  color <- black

@viewer "V" var=z size="100x100"
«script»
|] $ \ens -> do
        Just setupCfg <- getDynamic (ensembleSetup ens)
        case coContents setupCfg of
          Vertical panelsV -> getDynamic panelsV >>= \case
            [Panel _ bodyV] -> getDynamic bodyV >>= \case
              Vertical itemsV -> getDynamic itemsV >>= \case
                [ScriptBox UIScript{scriptCode}] -> do
                  -- `z` isn't in the declared `using dummy:Z` env, so the
                  -- preview itself should fail even with the bookkeeping
                  -- fallback (z is a real, ensemble-specific name, not
                  -- bookkeeping) -- but the splice substitution doesn't
                  -- care, since `z` *is* in the real viewer's own env.
                  setValue (source scriptCode) (CodeString "color <- if re z > 0 then white else black")
                  getDynamic (dyn scriptCode) >>= \case
                    Right _ -> expectationFailure "expected the preview typecheck to fail"
                    Left _  -> pure ()
                  rebuildWithCurrentSetup ens >>= \case
                    Left (_, err) -> expectationFailure ("expected the rebuild to succeed, got: " ++ err)
                    Right _       -> pure ()
                other -> expectationFailure ("expected a single ScriptBox, got " ++ show (length other))
              _ -> expectationFailure "expected the panel body to be Vertical"
            other -> expectationFailure ("expected a single Panel, got " ++ show (length other))
          _ -> expectationFailure "expected the Setup panel's top-level layout to be Vertical"

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

    it "builds a `using`-clause formula whose default expression uses the \
       \`escapes` keyword, even though `using` never declares the internal \
       \escape-radius name it resolves to -- falling back to offering the \
       \viewer bookkeeping names, the same way a real viewer body always can" $
      shouldBuild [r|
@setup "Setup"
stop : Boolean using z:C <- z escapes

@viewer "V" var=z size="100x100"
until «stop»:
  pass
color <- black
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
        title <- getDynamic (cvTitle viewer)
        title `shouldBe` Right "V"

    it "builds a `using`-clause formula whose default expression uses the \
       \`iterations`/`stuck` keywords" $
      shouldBuild [r|
@setup "Setup"
col : Color using z:C <- if stuck then black else rainbow(iterations / 20)

@viewer "V" var=z size="100x100"
iterate z -> z^2 until z escapes
color <- «col»
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
        title <- getDynamic (cvTitle viewer)
        title `shouldBe` Right "V"

    it "still rejects a formula field whose expression uses a variable \
       \missing from `using`, even once the bookkeeping fallback is tried" $
      [r|
@setup "Setup"
f : C using z:C <- w

@viewer "V" var=C size="100x100"
pass
|] `shouldFailWith` "doesn't typecheck"

    it "remembers its own source text, for a frontend's save path to write back out" $
      let src = "@viewer \"V\" var=z size=\"100x100\"\npass\n"
      in shouldBuild src $ \ens -> do
           msrc <- getDynamic (ensembleSourceScript ens)
           msrc `shouldBe` src

    it "encodeEnsemble writes a script-built ensemble's text back out verbatim" $
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

    it "builds a `prepare` block's output into a real PrepScript, merged into the \
       \viewer's own environment for its main code to reference" $
      shouldBuild [r|
@config "C"
myColor : Color <- white

@viewer "Voronoi" var=z size="150x150"
prepare computed : Color:
  computed <- myColor
color <- if |z| < 1 then black else computed
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
        cc <- getDynamic (cvCode viewer)
        case cc of
          Left (_, err) -> expectationFailure ("expected a build-able viewer, got: " ++ err)
          Right (SomeViewerWithContext _ mprep _) -> case mprep of
            Nothing -> expectationFailure "expected a PrepScript to have been built"
            Just _  -> pure ()

    it "accepts a `prepare` block whose output is assigned conditionally, in both \
       \branches of an if/else" $
      shouldBuild [r|
@viewer "V" var=z size="100x100"
prepare computed : Color:
  if 1 < 2:
    computed <- black
  else:
    computed <- white
color <- computed
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
        cc <- getDynamic (cvCode viewer)
        case cc of
          Left (_, err) -> expectationFailure ("expected a build-able viewer, got: " ++ err)
          Right (SomeViewerWithContext _ mprep _) -> case mprep of
            Nothing -> expectationFailure "expected a PrepScript to have been built"
            Just _  -> pure ()

    it "rejects a `prepare` output that's never assigned" $
      [r|
@viewer "V" var=z size="100x100"
prepare computed : Color:
  pass
color <- black
|] `shouldFailWith` "is never assigned"

    it "rejects a `prepare` output read before it's assigned" $
      [r|
@viewer "V" var=z size="100x100"
prepare computed : Color:
  color <- computed
  computed <- black
color <- black
|] `shouldFailWith` "is read before"

    it "rejects a `prepare` output only assigned in one branch of an if/else" $
      [r|
@viewer "V" var=z size="100x100"
prepare computed : Color:
  if 1 < 2:
    computed <- black
  else:
    pass
color <- black
|] `shouldFailWith` "is never assigned"

    it "rejects a `prepare` output only assigned inside a loop (conservative: no \
       \credit for a loop body even though this one always runs)" $
      [r|
@viewer "V" var=z size="100x100"
prepare computed : Color:
  repeat:
    computed <- black
  while false
color <- black
|] `shouldFailWith` "is never assigned"

    it "rejects two `prepare` outputs sharing a name" $
      [r|
@viewer "V" var=z size="100x100"
prepare computed : Color, computed : Color:
  computed <- black
color <- black
|] `shouldFailWith` "more than once"

    it "rejects a `prepare` output whose name collides with an ensemble `@config` variable" $
      [r|
@config "C"
computed : Color <- white

@viewer "V" var=z size="100x100"
prepare computed : Color:
  computed <- black
color <- computed
|] `shouldFailWith` "defined twice"

    it "splices a `code «name»:` declaration -- a whole block of statements \
       \-- into a viewer's own body" $
      shouldBuild [r|
@setup "Setup"
code script:
  iterate z -> z^2 + i until z escapes
  color <- if stuck then black else white

@viewer "V" var=z size="100x100"
«script»
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
        cc <- getDynamic (cvCode viewer)
        case cc of
          Left (_, err) -> expectationFailure ("expected the spliced code to build, got: " ++ err)
          Right _        -> pure ()

    it "rejects a `code «name»:` declaration whose body references an \
       \undeclared variable, once spliced into a viewer" $
      [r|
@setup "Setup"
code script:
  color <- badvar

@viewer "V" var=z size="100x100"
«script»
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

    it "accepts a `using`-clause formula whose default expression uses the \
       \`escapes`/`iterations`/`stuck` bookkeeping keywords, agreeing with \
       \the real build's own bookkeeping fallback" $
      agree [r|
@setup "Setup"
stop : Boolean using z:C <- z escapes
col : Color using z:C <- if stuck then black else rainbow(iterations / 20)

@viewer "V" var=z size="100x100"
until «stop»:
  pass
color <- «col»
|]

    it "still rejects a `using`-clause formula missing an ordinary variable, \
       \agreeing with the real build even once the bookkeeping fallback is tried" $
      agree [r|
@setup "Setup"
f : C using z:C <- w

@viewer "V" var=C size="100x100"
pass
|]

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

    -- Unlike tool code (above), a `prepare` block's own code *is*
    -- eagerly checked by buildEnsembleFromScript (it's forced as part
    -- of the main viewer's own ComplexViewer code field), so checkScript
    -- and buildEnsembleFromScript should agree on every case here --
    -- no "new coverage" asymmetry to call out.

    it "accepts a `prepare` block whose declared output is correctly assigned \
       \and used by the main viewer code" $
      agree [r|
@viewer "V" var=z size="100x100"
prepare computed : Color:
  computed <- black
color <- computed
|]

    it "catches a `prepare` output that's never assigned" $
      agree [r|
@viewer "V" var=z size="100x100"
prepare computed : Color:
  pass
color <- black
|]

    it "catches a `prepare` output read before it's assigned" $
      agree [r|
@viewer "V" var=z size="100x100"
prepare computed : Color:
  color <- computed
  computed <- black
color <- black
|]

    it "catches two `prepare` outputs sharing a name" $
      agree [r|
@viewer "V" var=z size="100x100"
prepare computed : Color, computed : Color:
  computed <- black
color <- black
|]

    it "catches a `prepare` output colliding with an ensemble `@config` variable" $
      agree [r|
@config "C"
computed : Color <- white

@viewer "V" var=z size="100x100"
prepare computed : Color:
  computed <- black
color <- computed
|]

    -- A `code «name»:` declaration's own body is already fully resolved
    -- into the splicing viewer's own ParsedCode by parseScript itself
    -- (splicing happens at parse time), so checkViewer's existing
    -- vfCode check already covers it -- no extra wiring needed, and no
    -- "new coverage" asymmetry to call out, same as `prepare`.

    it "accepts a `code «name»:` declaration spliced into a viewer" $
      agree [r|
@setup "Setup"
code script:
  iterate z -> z^2 + i until z escapes
  color <- if stuck then black else white

@viewer "V" var=z size="100x100"
«script»
|]

    it "catches a `code «name»:` declaration referencing an undeclared \
       \variable, once spliced into a viewer" $
      agree [r|
@setup "Setup"
code script:
  color <- badvar

@viewer "V" var=z size="100x100"
«script»
|]

  describe "@common stanzas" $ do

    let accepts :: String -> Expectation
        accepts src = case checkScript src of
          Left (_, err) -> expectationFailure ("expected checkScript to accept this, got: " ++ err)
          Right ()       -> pure ()

        rejects :: String -> String -> Expectation
        rejects src needle = case checkScript src of
          Left (_, err) -> err `shouldSatisfy` isInfixOf needle
          Right ()       -> expectationFailure "expected checkScript to reject this, but it accepted it"

        viewerCode :: String -> IO String
        viewerCode src = build src >>= \case
          Left err -> fail ("expected success, got error: " ++ err)
          Right ens -> do
            [viewer] <- getDynamic (ensembleViewers ens)
            getDynamic (cvCode viewer) >>= \case
              Left (_, err) -> fail ("expected the viewer's code to build, got: " ++ err)
              Right (SomeViewerWithContext _ _ code) -> pure (pprintCode code)

    it "makes its definitions available to viewers and tools" $ do
      let src = [r|
@config "Configuration"
a : C as "a =" <- 1

@common
let f(z) = z^3 - a
step := 1

@viewer "V" var=z size="100x100"
iterate z -> z - step * f(z) / f'(z) until f(z) vanishes

@tool "T" for="V"
on click:
  z <- z - step * f(z) / f'(z)
|]
      accepts src
      code <- viewerCode src
      code `shouldSatisfy` isInfixOf "let `step` = 1"

    it "can come after the stanzas that use it, and can be split up" $
      accepts [r|
@viewer "V" var=z size="100x100"
color <- if g(z) < 1 then black else white

@common
let f(z) = |z|

@common
let g(w) = f(w) / 2
|]

    it "is available in preparation scripts" $
      accepts [r|
@common
let f(z) = |z|

@viewer "V" var=z size="100x100"
prepare r : R:
  r <- f(-2)
color <- if |z| < r then black else white
|]

    it "is typechecked where it is used" $
      rejects [r|
@common
k := 2

@viewer "V" var=z size="100x100"
pass

@tool "T" for="V"
k : Z as "k" <- 1
on click:
  k <- k + 1
|] "The variable k is already defined"

    it "only includes the definitions a script uses" $ do
      -- `g` mentions the viewer's `z`, which neither the preparation
      -- script nor the refresh handler can see; neither uses `g`.
      accepts [r|
@common
let g(w) = w + z
c := 2

@viewer "V" var=z size="100x100"
prepare r : R:
  r <- c
color <- if |g(z)| < r then black else white

@tool "T" for="V"
on refresh:
  erase
on click:
  draw point at g(z)
|]
      -- ...but a script that does use it still gets the error
      rejects [r|
@common
let g(w) = w + z

@viewer "V" var=z size="100x100"
prepare r : R:
  r <- |g(1)|
color <- if r < 1 then black else white
|] "No variable named z"

    it "includes definitions used through other definitions" $ do
      code <- viewerCode [r|
@common
a := 3
unused := 4
let f(t) = a t
let g(t) = f(t) + 1

@viewer "V" var=z size="100x100"
color <- if |g(z)| < 1 then black else white
|]
      code `shouldSatisfy` isInfixOf "let `a` = 3"
      code `shouldNotSatisfy` isInfixOf "unused"

    it "can only contain declarations and function definitions" $ do
      rejects [r|
@common
erase

@viewer "V" var=z size="100x100"
pass
|] "can only contain variable declarations"
      rejects [r|
@common
let f(z) = z
color <- black

@viewer "V" var=z size="100x100"
pass
|] "can only contain variable declarations"

    it "takes no attributes" $
      rejects [r|
@common bogus=1
let f(z) = z

@viewer "V" var=z size="100x100"
pass
|] "bogus"
