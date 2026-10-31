{-# language QuasiQuotes, OverloadedStrings #-}
module Actor.EnsembleBuildSpec (spec) where

import Test.Hspec

import Actor.Ensemble
import Actor.Viewer (SomeViewerWithContext(..))
import Actor.Viewer.Complex (ComplexViewer(..))
import Actor.Tool (Tool(..), ToolInfo(..))
import Actor.Layout (Dimensions(..), Layout(..), Label(..), UIVariable(..), TabItem(..))
import Data.Color (red, blue)
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
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))


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
@viewer "Mandelbrot Set" var=C
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
@viewer "Mandelbrot Set" var=C
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
@viewer "Mandelbrot Set" var=C pixelVar=px
z : C <- 0
iterate z -> z^2 + C until z escapes

@tool "Trace" for="Mandelbrot Set" drawLayer="5" refreshOnActivate="false" refreshCanUpdate="true"
on refresh:
  pass
|] $ \ens -> do
        [viewer] <- getDynamic (ensembleViewers ens)
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
@viewer "V" var=z
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
@viewer "V" var=z
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
@viewer "Only One" var=z
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

@config "Dynamics"
c : Z <- 3

@viewer "V" var=z
pass
|] $ \ens -> do
        mcfg <- getDynamic (ensembleConfiguration ens)
        case mcfg of
          Nothing -> expectationFailure "expected a Configuration"
          -- One tab per title, in order of first appearance, with
          -- stanzas sharing a title merged into one tab.
          Just cfg -> case coContents cfg of
            Tabbed tabsV -> do
              tabs <- getDynamic tabsV
              labels <- mapM (getDynamic . tiLabel) tabs
              [ l | Label l <- labels ] `shouldBe` ["Dynamics", "Coloring", "Iteration settings"]
              pages <- mapM (getDynamic . tiBody) tabs
              sizes <- mapM (\case { Vertical v -> length <$> getDynamic v; _ -> pure (-1) }) pages
              sizes `shouldBe` [2, 1, 3]
            _ -> expectationFailure "expected the top-level Configuration layout to be Tabbed"

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
          -- Two tabs, not one: every ensemble now also gets an
          -- automatically-provided "Iteration settings" tab
          -- (max_iters/escape_radius/vanishing_radius) appended after whatever
          -- the script's own @config declares.
          Just cfg -> case coContents cfg of
            Tabbed tabsV -> getDynamic tabsV >>= \case
              [TabItem _ bodyV, _implicitTab] -> getDynamic bodyV >>= \case
                Vertical itemsV -> getDynamic itemsV >>= \case
                  [Horizontal rowV, _declItem] -> do
                    rowItems <- getDynamic rowV
                    length rowItems `shouldBe` 2
                  other -> expectationFailure ("unexpected tab body items: " ++ show (length other))
                _ -> expectationFailure "expected the tab body to be Vertical"
              other -> expectationFailure ("expected two tabs (the script's own, plus the implicit one), got " ++ show (length other))
            _ -> expectationFailure "expected the top-level Configuration layout to be Tabbed"

    it "builds consecutive tab: blocks into one Tabbed layout" $
      shouldBuild [r|
@config "Configuration"
tab "First":
  a : Z <- 1
  b : Z <- 2
tab "Second":
  c : Z <- 3
|] $ \ens -> do
        mcfg <- getDynamic (ensembleConfiguration ens)
        case mcfg of
          Nothing -> expectationFailure "expected a Configuration"
          Just cfg -> case coContents cfg of
            Tabbed tabsV -> getDynamic tabsV >>= \case
              [TabItem _ bodyV, _implicitTab] -> getDynamic bodyV >>= \case
                Vertical itemsV -> getDynamic itemsV >>= \case
                  [Tabbed innerTabsV] -> do
                    tabs <- getDynamic innerTabsV
                    labels <- mapM (getDynamic . tiLabel) tabs
                    [ l | Label l <- labels ] `shouldBe` ["First", "Second"]
                    pages <- mapM (getDynamic . tiBody) tabs
                    sizes <- mapM (\case { Vertical v -> length <$> getDynamic v; _ -> pure (-1) }) pages
                    sizes `shouldBe` [2, 1]
                  other -> expectationFailure ("unexpected tab body items: " ++ show (length other))
                _ -> expectationFailure "expected the tab body to be Vertical"
              other -> expectationFailure ("expected two tabs (the script's own, plus the implicit one), got " ++ show (length other))
            _ -> expectationFailure "expected the top-level Configuration layout to be Tabbed"

    it "makes a @config color picker reactive to edits \
       \(regression: a frozen Mapped used to silently ignore them)" $
      shouldBuild [r|
@config "Configuration"
c : Color <- red
|] $ \ens -> do
        mcfg <- getDynamic (ensembleConfiguration ens)
        case mcfg of
          Nothing -> expectationFailure "expected a Configuration"
          Just cfg -> case coContents cfg of
            Tabbed tabsV -> getDynamic tabsV >>= \case
              [TabItem _ bodyV, _implicitTab] -> getDynamic bodyV >>= \case
                Vertical itemsV -> getDynamic itemsV >>= \case
                  [ColorPicker _ _ colorP] -> do
                    getDynamic (dyn colorP) >>= (`shouldBe` Right red)
                    setValue (source colorP) "blue"
                    getDynamic (dyn colorP) >>= (`shouldBe` Right blue)
                  other -> expectationFailure ("unexpected tab body items: " ++ show (length other))
                _ -> expectationFailure "expected the tab body to be Vertical"
              other -> expectationFailure ("expected two tabs (the script's own, plus the implicit one), got " ++ show (length other))
            _ -> expectationFailure "expected the top-level Configuration layout to be Tabbed"

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
            Tabbed tabsV -> getDynamic tabsV >>= \case
              [TabItem _ bodyV, _implicitTab] -> getDynamic bodyV >>= \case
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
                  other -> expectationFailure ("unexpected tab body items: " ++ show (length other))
                _ -> expectationFailure "expected the tab body to be Vertical"
              other -> expectationFailure ("expected two tabs (the script's own, plus the implicit one), got " ++ show (length other))
            _ -> expectationFailure "expected the top-level Configuration layout to be Tabbed"

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
            Tabbed tabsV -> getDynamic tabsV >>= \case
              [TabItem _ bodyV, _implicitTab] -> getDynamic bodyV >>= \case
                Vertical itemsV -> getDynamic itemsV >>= \case
                  [TextBox _ UIVariable{exprValue}] -> do
                    initialText <- getDynamic (source exprValue)
                    initialText `shouldSatisfy` notElem 'e'
                    getDynamic (dyn exprValue) >>= \case
                      Left err -> expectationFailure ("expected the initial value to parse, got: " ++ err)
                      Right (SomeValue EmptyEnvProxy RealType v) ->
                        evaluateInContext EmptyContext v `shouldBe` 0.05
                      Right _ -> expectationFailure "expected an empty-environment Real value"
                  other -> expectationFailure ("unexpected tab body items: " ++ show (length other))
                _ -> expectationFailure "expected the tab body to be Vertical"
              other -> expectationFailure ("expected two tabs (the script's own, plus the implicit one), got " ++ show (length other))
            _ -> expectationFailure "expected the top-level Configuration layout to be Tabbed"

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
            Tabbed tabsV -> getDynamic tabsV >>= \case
              [TabItem _ bodyV, _implicitTab] -> getDynamic bodyV >>= \case
                Vertical itemsV -> getDynamic itemsV >>= \case
                  [TextBox _ UIVariable{exprValue}] ->
                    getDynamic (dyn exprValue) >>= \case
                      Left err -> expectationFailure ("expected the initial value to parse, got: " ++ err)
                      Right (SomeValue EmptyEnvProxy (ListType IntegerType) v) ->
                        evaluateInContext EmptyContext v `shouldBe` [1, 2, 3]
                      Right _ -> expectationFailure "expected an empty-environment List of Integer value"
                  other -> expectationFailure ("unexpected tab body items: " ++ show (length other))
                _ -> expectationFailure "expected the tab body to be Vertical"
              other -> expectationFailure ("expected two tabs (the script's own, plus the implicit one), got " ++ show (length other))
            _ -> expectationFailure "expected the top-level Configuration layout to be Tabbed"

    it "automatically provides max_iters/escape_radius/vanishing_radius in an \
       \\"Iteration settings\" tab, with the right types and defaults, \
       \even when the script declares no @config at all" $
      shouldBuild [r|
@viewer "V" var=z
pass
|] $ \ens -> do
        Just cfg <- getDynamic (ensembleConfiguration ens)
        case coContents cfg of
          Tabbed tabsV -> getDynamic tabsV >>= \case
            [TabItem titleV bodyV] -> do
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
                _ -> expectationFailure "expected the tab body to be Vertical"
            other -> expectationFailure ("expected a single implicit tab, got " ++ show (length other))
          _ -> expectationFailure "expected the top-level Configuration layout to be Tabbed"

    it "lets an explicit `limit=`/`escape=`/`vanish=`/`pixelVar=` override \
       \still point at the script's own custom-named variable instead of \
       \the automatic max_iters/escape_radius/vanishing_radius/pixel_size" $
      shouldBuild [r|
@config "Configuration"
myLimit : Z <- 50
myEscape : R <- 5
myVanish : R <- 0.01

@viewer "V" var=z limit="myLimit" escape="myEscape" vanish="myVanish" pixelVar="myPixel"
color <- if myPixel > 0 then black else white
|] $ \_ -> pure ()

    it "a user-declared `max_iters` collides cleanly with the automatic \
       \one, same as any other duplicate config name" $
      [r|
@config "Configuration"
max_iters : Z <- 50

@viewer "V" var=z
pass
|] `shouldFailWith` "defined twice"

    it "a nullary event handler (`on refresh:`) can now reference the \
       \automatically-provided `pixel_size` (regression: it previously had \
       \no way to name the per-pixel scale at all for this handler kind)" $
      shouldBuild [r|
@viewer "V" var=z
color <- black

@tool "T" shortcut="t"
on refresh:
  scale : R <- pixel_size * 2
|] $ \_ -> pure ()

    it "remembers its own source text, for a frontend's save path to write back out" $
      let src = "@viewer \"V\" var=z\npass\n"
      in shouldBuild src $ \ens -> do
           msrc <- getDynamic (ensembleSourceScript ens)
           msrc `shouldBe` src

    it "encodeEnsemble writes a script-built ensemble's text back out verbatim" $
      let src = "@viewer \"V\" var=z\npass\n"
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
      let src = "@viewer \"V\" var=z\npass\n"
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

    it "keeps a project file's other top-level keys (e.g. a frontend's \
       \`wx-properties:`) and writes them back out unchanged" $ do
      dir <- getTemporaryDirectory
      let path = dir </> "fractalstream-frontend-properties-test.fs"
      writeFile path (unlines
        [ "script: |"
        , "  @viewer \"V\" var=z"
        , "  pass"
        , "wx-properties:"
        , "  viewers:"
        , "    V:"
        , "      width: 320"
        , "      center: [1.5, -2]"
        ])
      result <- parseEnsembleFromFile path
      removeFile path
      case result of
        Left err -> expectationFailure err
        Right ens -> do
          props <- getDynamic (ensembleFrontendProperties ens)
          KM.keys props `shouldBe` ["wx-properties"]
          bytes <- encodeEnsemble ens
          case YAML.decodeEither' bytes of
            Right (Object o) ->
              KM.lookup "wx-properties" o `shouldBe` KM.lookup "wx-properties" props
            _ -> expectationFailure "expected a YAML object"

    it "gives viewers default window geometry, staggering each one's \
       \position 30 pixels further than the last" $
      shouldBuild [r|
@viewer "A"
pass

@viewer "B"
pass

@viewer "C"
pass
|] $ \ens -> do
        viewers <- getDynamic (ensembleViewers ens)
        positions <- mapM (getDynamic . cvPosition) viewers
        positions `shouldBe` map Dimensions [(100, 100), (130, 130), (160, 160)]
        sizes <- mapM (getDynamic . cvSize) viewers
        sizes `shouldBe` replicate 3 (Dimensions (600, 600))
        resizable <- mapM (getDynamic . cvCanResize) viewers
        resizable `shouldBe` replicate 3 True

    it "rejects a tool targeting an unknown viewer" $
      [r|
@viewer "A" var=z
pass

@tool "T" for="B"
on refresh:
  pass
|] `shouldFailWith` "targets an unknown viewer `B`"

    it "rejects a tool with no `for=` when there's more than one viewer" $
      [r|
@viewer "A" var=z
pass

@viewer "B" var=z
pass

@tool "T"
on refresh:
  pass
|] `shouldFailWith` "doesn't specify `for=`"

    it "rejects two viewers sharing a title" $
      [r|
@viewer "A" var=z
pass

@viewer "A" var=w
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

@viewer "V" var=z
pass
|] `shouldFailWith` "out of range"

    it "rejects a viewer body that references an undeclared variable" $
      [r|
@viewer "V" var=z
w <- w + 1
|] `shouldFailWith` "No variable named"

    it "builds a `prepare` block's output into a real PrepScript, merged into the \
       \viewer's own environment for its main code to reference" $
      shouldBuild [r|
@config "C"
myColor : Color <- white

@viewer "Voronoi" var=z
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
@viewer "V" var=z
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
@viewer "V" var=z
prepare computed : Color:
  pass
color <- black
|] `shouldFailWith` "is never assigned"

    it "rejects a `prepare` output read before it's assigned" $
      [r|
@viewer "V" var=z
prepare computed : Color:
  color <- computed
  computed <- black
color <- black
|] `shouldFailWith` "is read before"

    it "rejects a `prepare` output only assigned in one branch of an if/else" $
      [r|
@viewer "V" var=z
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
@viewer "V" var=z
prepare computed : Color:
  repeat:
    computed <- black
  while false
color <- black
|] `shouldFailWith` "is never assigned"

    it "rejects two `prepare` outputs sharing a name" $
      [r|
@viewer "V" var=z
prepare computed : Color, computed : Color:
  computed <- black
color <- black
|] `shouldFailWith` "more than once"

    it "rejects a `prepare` output whose name collides with an ensemble `@config` variable" $
      [r|
@config "C"
computed : Color <- white

@viewer "V" var=z
prepare computed : Color:
  computed <- black
color <- computed
|] `shouldFailWith` "defined twice"

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
@viewer "Mandelbrot Set" var=C
z : C <- 0
iterate z -> z^2 + C until z escapes
color <- if stuck then black else rainbow(iterations / 20)
|]

    it "catches a viewer body referencing an undeclared variable, live, \
       \without building an Ensemble" $
      checkFailsWith [r|
@viewer "V" var=z
w <- w + 1
|] "No variable named"

    it "catches a viewer body referencing a misspelled `@config` variable" $
      checkFailsWith [r|
@config "C"
radius : R <- 2

@viewer "V" var=z
color <- if |z| > raduis then black else white
|] "No variable named"

    it "accepts a viewer body that correctly references a `@config` variable" $
      checkAccepts [r|
@config "C"
radius : R <- 2

@viewer "V" var=z
color <- if |z| > radius then black else white
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

@viewer "V" var=z
pass
|] "out of range"

    it "still catches cross-stanza errors (unchanged from before this was extended)" $ do
      checkFailsWith [r|
@viewer "A" var=z
pass

@tool "T" for="B"
on refresh:
  pass
|] "targets an unknown viewer `B`"
      checkFailsWith [r|
@viewer "A" var=z
pass

@viewer "A" var=w
pass
|] "share the title `A`"

    it "agrees with buildEnsembleFromScript on acceptance/rejection, across a \
       \mix of valid and invalid scripts" $ do
      agree [r|
@viewer "Mandelbrot Set" var=C
z : C <- 0
iterate z -> z^2 + C until z escapes
|]
      agree [r|
@config "C"
radius : R <- 2

@viewer "V" var=z
color <- if |z| > radius then black else white
|]
      agree [r|
@viewer "V" var=z
w <- w + 1
|]
      agree [r|
@config "C"
foo : Z <- "hello"
|]
      agree [r|
@config "C"
foo : Z as "Pick" choice {"a", "b"} <- 5

@viewer "V" var=z
pass
|]
      agree [r|
@common
let f(z) = z^2 + C

@viewer "Mandelbrot" var=C
z : C <- 0
iterate z -> f(z) until z escapes
|]

    it "catches a tool body referencing an undeclared variable" $
      checkFailsWith [r|
@viewer "V" var=z
pass

@tool "T" for="V"
on click:
  draw point at badvar
|] "No variable named"

    it "accepts a tool body that correctly references a `@config` variable" $
      checkAccepts [r|
@config "C"
center : C <- 0

@viewer "V" var=z
pass

@tool "T" for="V"
on click:
  draw point at center
|]

    it "accepts a tool body that references the tool's own declaration" $
      checkAccepts [r|
@viewer "V" var=z
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

@viewer "V" var=z
pass

@tool "T" for="V"
center : C <- 1
on click:
  draw point at center
|] "is defined twice"

    it "catches a tool error at build time, not only when the tool is first used" $
      agree [r|
@viewer "V" var=z
pass

@tool "T" for="V"
on click:
  draw point at badvar
|]

    -- A `prepare` block's own code is also checked by
    -- buildEnsembleFromScript, so the two should agree here too.

    it "accepts a `prepare` block whose declared output is correctly assigned \
       \and used by the main viewer code" $
      agree [r|
@viewer "V" var=z
prepare computed : Color:
  computed <- black
color <- computed
|]

    it "catches a `prepare` output that's never assigned" $
      agree [r|
@viewer "V" var=z
prepare computed : Color:
  pass
color <- black
|]

    it "catches a `prepare` output read before it's assigned" $
      agree [r|
@viewer "V" var=z
prepare computed : Color:
  color <- computed
  computed <- black
color <- black
|]

    it "catches two `prepare` outputs sharing a name" $
      agree [r|
@viewer "V" var=z
prepare computed : Color, computed : Color:
  computed <- black
color <- black
|]

    it "catches a `prepare` output colliding with an ensemble `@config` variable" $
      agree [r|
@config "C"
computed : Color <- white

@viewer "V" var=z
prepare computed : Color:
  computed <- black
color <- computed
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

@viewer "V" var=z
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
@viewer "V" var=z
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

@viewer "V" var=z
prepare r : R:
  r <- f(-2)
color <- if |z| < r then black else white
|]

    it "is typechecked where it is used" $
      rejects [r|
@common
k := 2

@viewer "V" var=z
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

@viewer "V" var=z
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

@viewer "V" var=z
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

@viewer "V" var=z
color <- if |g(z)| < 1 then black else white
|]
      code `shouldSatisfy` isInfixOf "let `a` = 3"
      code `shouldNotSatisfy` isInfixOf "unused"

    it "can only contain declarations and function definitions" $ do
      rejects [r|
@common
erase

@viewer "V" var=z
pass
|] "can only contain variable declarations"
      rejects [r|
@common
let f(z) = z
color <- black

@viewer "V" var=z
pass
|] "can only contain variable declarations"

    it "takes no attributes" $
      rejects [r|
@common bogus=1
let f(z) = z

@viewer "V" var=z
pass
|] "bogus"

    it "can define code for viewers and tools to run" $ do
      let src = [r|
@common
define shade(c):
  if stuck:
    color <- black
  else:
    color <- blend(iterations / 20, light c, dark c)
define trace:
  erase
  repeat up to 10 times:
    draw point at z
    z <- z^2 + 0.25
  until z escapes

@viewer "V" var=z
iterate z -> z^2 + 0.25 until z escapes
run shade(blue)

@tool "T" for="V"
on click:
  run trace
|]
      accepts src
      code <- viewerCode src
      code `shouldSatisfy` isInfixOf "blend("
      -- `trace` uses drawing commands, but only the tool runs it
      code `shouldNotSatisfy` isInfixOf "point"
