{-# language OverloadedStrings, TemplateHaskell #-}
module Actor.Ensemble
  ( Ensemble(..)
  , NamedScenario(..)
  , parseEnsembleFromFile
  , buildEnsembleFromScript
  , runEnsemble
  , runEnsembleFromSetup

  , contextValuesChanged
  , layoutToArgs
  , buildToolContext

  , BadEnsemble(..)

  , allTemplates

  ) where

import FractalStream.Prelude

import Data.DynamicValue
import Actor.UI
import Actor.Configuration
import Actor.Layout
import Actor.Event
import Actor.Tool (Tool(..), ToolInfo(..))
import Actor.Viewer
import Actor.Viewer.Complex
import Actor.Script
import Language.Environment
import Language.Value hiding (Join)
import Language.Value.Parser
import Language.Typecheck (TC(..), ppError)
import Language.Code.Parser (Splices(..), noSplices, ParsedCode)
import Language.Parser.SourceRange

import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Yaml as YAML
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub, (\\))
import Data.Typeable (Typeable)

import Language.Code (usedVarsInCode, SomeCode(..))
import Language.Value.Evaluator (evaluate, evaluateInContext)
import Development.IncludeFile

import Control.Exception (Exception, IOException, throwIO, try)

import Data.Codec

data BadEnsemble = BadEnsemble String deriving Show
instance Exception BadEnsemble


-- | A named, headless-runnable sequence of commands, embedded directly in
-- a project file under a top-level @scenarios:@ list -- e.g. so a CI run
-- can do @fsrun project.yaml --scenario NAME@ instead of
-- pointing at a separate commands file. The commands themselves are kept
-- as raw text here: the scenario language's parser
-- (@Scenario.parseScenario@) lives in @fractalstream-ui-headless@, not
-- here, since it's specific to that frontend, not a thing every consumer
-- of an 'Ensemble' needs to know about.
data NamedScenario = NamedScenario
  { nsName     :: Variable String
  , nsCommands :: Variable String
  }

instance Codec NamedScenario where
  codec = do
    name     <-nsName-<     key "name"
    commands <-nsCommands-< key "commands"
    build NamedScenario name commands

data Ensemble = Ensemble
  { ensembleSetup         :: Variable (Maybe Configuration)
  , ensembleSplices       :: Dynamic (Either String Splices)
  , ensembleConfiguration :: Variable (Maybe Configuration)
  , ensembleViewers       :: Variable [ComplexViewer]
  , ensembleScenarios     :: Variable [NamedScenario]
  }

instance Codec Ensemble where
  codec = do
    setup <-ensembleSetup-< optionalField "setup"
      (newVariable Nothing) (fmap isNothing . getDynamic) $ do
      codecWith (pure . pure . pure $ noSplices)

    splices <-ensembleSplices-< purely $ \use -> dyn (use setup) >>= \case
      Nothing                -> pure (pure noSplices)
      Just Configuration{..} -> layoutToSplices coContents

    config <-ensembleConfiguration-< optionalField "configuration"
      (newVariable Nothing) (fmap isNothing . getDynamic) $
      codecWith splices

    context <- purely $ \use -> dyn (use config) >>= \case
      Nothing                -> pure (Right $ SomeContext EmptyContext)
      Just Configuration{..} -> layoutContext coContents

    ctx <- purely $ \use -> (use context, use splices)

    -- NOTE: deliberately no fallback fragment for "neither `viewer:` nor
    -- `viewers:` present" -- a project with no viewers at all isn't a
    -- legitimate case, so that absence should surface as a decode error
    -- rather than silently producing an empty-viewer ensemble.
    viewers <-ensembleViewers-< newOf $ match
      [ Fragment (:[]) (\case { [x] -> Just x; _ -> Nothing }) $ field "viewer" (codecWith ctx)
      , Fragment id Just $ field "viewers" (codecWith ctx)
      ]

    scenarios <-ensembleScenarios-< optionalField "scenarios"
      (newVariable []) (fmap null . getDynamic) codec

    build Ensemble setup splices config viewers scenarios

contextValuesChanged :: SomeContext DynamicValue -> Dynamic ()
contextValuesChanged (SomeContext ctx0) = go ctx0
  where
    go :: forall env. Context DynamicValue env -> Dynamic ()
    go = \case
      Bind _ _ v ctx' -> (<>) <$> (const () <$> v) <*> go ctx'
      EmptyContext    -> pure ()

-- | The full variable context a tool's event handlers see: its own
-- @configuration:@ panel's variables (if it has one) combined with the
-- ensemble-wide context, matching what every frontend needs to build
-- before calling a 'Viewer''s @vBuildToolHandler@. Shared by
-- @fractalstream-ui-wx@ and @fractalstream-ui-headless@ so the two
-- frontends can't silently drift on how a tool's context is assembled.
buildToolContext :: SomeContext EventArgument_
                  -> Maybe Configuration
                  -> IO (Either String (SomeContext EventArgument_))
buildToolContext ensembleCtx = \case
  Nothing -> pure (Right ensembleCtx)
  Just tcfg -> do
    toolContext0 <- layoutToArgs (coContents tcfg)
    pure $ case toolContext0 <> SomeContext' (Right ensembleCtx) of
      SomeContext' e -> e

-- | Load an 'Ensemble' from a project file. Reports a missing\/unreadable
-- file the same way every other failure here is reported (as a 'Left'),
-- rather than letting 'BS.readFile' throw an uncaught 'IOException' --
-- the project path comes from user input (an open dialog, a CLI
-- argument, ...), so a bad path is an ordinary, expected failure here,
-- not a programming error.
parseEnsembleFromFile :: FilePath -> IO (Either String Ensemble)
parseEnsembleFromFile path = try (BS.readFile path) >>= \case
  Left (e :: IOException) -> pure (Left ("could not read " ++ path ++ ": " ++ show e))
  Right contents -> do
    BS.length contents `seq` pure ()
    -- A project written as a single unified script carries its whole
    -- text under one top-level `script:` key, tried before falling back
    -- to the classic `setup:`/`configuration:`/`viewer(s):` shape -- so
    -- existing projects keep loading exactly as before, unchanged.
    case YAML.decodeEither' contents of
      Right (Object o) | Just (String scriptText) <- KM.lookup "script" o ->
        buildEnsembleFromScript (Text.unpack scriptText) >>= \case
          Left err  -> pure (Left (formatScriptError (Text.unpack scriptText) err))
          Right ens -> attachScenarios o ens
      _ -> deserializeYAML contents

-- | A unified-script project can still carry a sibling top-level
-- `scenarios:` key. Reading it has nothing to do with the unified-script
-- grammar or the scenario *command* language (that parser lives in
-- fractalstream-ui-headless, not here) -- 'NamedScenario' only ever holds
-- a scenario's name and command text as opaque strings, exactly like the
-- classic YAML path already does, so it's just another key to read off
-- the same decoded document.
attachScenarios :: KM.KeyMap Aeson.Value -> Ensemble -> IO (Either String Ensemble)
attachScenarios o ens = case KM.lookup "scenarios" o of
  Nothing -> pure (Right ens)
  Just scenariosValue -> deserializeValue scenariosValue >>= \case
    Left err -> pure (Left ("could not read `scenarios:`: " ++ err))
    Right scenarios -> do
      setValue' (ensembleScenarios ens) scenarios
      pure (Right ens)

-- | Render a unified-script build error the same way every other
-- source-range-tagged parse\/typecheck error in this codebase is shown:
-- the offending span of the original text, followed by the message.
formatScriptError :: String -> (SourceRange, String) -> String
formatScriptError src (range, msg) = unlines (pp (Indicated range src) ++ [""] ++ [msg])

------------------------------------------------------------
-- Unified project script -> Ensemble (Layers 3/4)
--
-- Resolves a parsed script's cross-stanza references (a tool's @for=@
-- target, a viewer's @limit=@\/@escape=@\/@vanish=@ attributes naming a
-- config variable) and compiles the result into a real 'Ensemble', built
-- directly out of 'Layout'\/'Configuration'\/'ComplexViewer'\/'Tool'
-- values rather than by round-tripping through YAML text -- so that a
-- viewer's or tool's body code, already parsed once in 'Actor.Script's
-- single Earley pass, is never re-serialized to a string and
-- re-tokenized. Lives here (rather than in a separate module importing
-- 'Actor.Ensemble') because it needs to construct 'Ensemble' values
-- directly.
------------------------------------------------------------

-- | Parse a unified project script and compile it into a runnable
-- 'Ensemble', resolving every cross-stanza reference along the way.
buildEnsembleFromScript :: String -> IO (Either (SourceRange, String) Ensemble)
buildEnsembleFromScript scriptText = case parseScript scriptText of
  Left err -> pure (Left err)
  Right stanzas -> do
    let setupGroups   = [ (t, b) | SetupStanza t b <- stanzas ]
        configGroups  = [ (t, b) | ConfigStanza t b <- stanzas ]
        viewers       = mapMaybe viewerFields stanzas
        tools         = mapMaybe toolFields stanzas
        viewerTitles  = [ vfTitle v | v <- viewers ]

    case checkTitlesAndTargets viewerTitles tools of
      Left err -> pure (Left err)
      Right () -> buildWithValidTargets setupGroups configGroups viewers tools viewerTitles

buildWithValidTargets :: [(String, [ConfigStmt])]
                       -> [(String, [ConfigStmt])]
                       -> [ViewerFields]
                       -> [ToolFields]
                       -> [String]
                       -> IO (Either (SourceRange, String) Ensemble)
buildWithValidTargets setupGroups configGroups viewers tools viewerTitles = do
  eSetupConfig <- buildMaybeConfiguration "Setup" setupGroups
  case eSetupConfig of
    Left err -> pure (Left err)
    Right setupConfig -> do
      eConfigConfig <- buildMaybeConfiguration "Configuration" configGroups
      case eConfigConfig of
        Left err -> pure (Left err)
        Right configConfig -> do
          setupVar  <- newVariable setupConfig
          configVar <- newVariable configConfig

          let splicesDyn :: Dynamic (Either String Splices)
              splicesDyn = dyn setupVar >>= \case
                Nothing                -> pure (Right noSplices)
                Just Configuration{..} -> layoutToSplices coContents

              contextDyn :: Dynamic (Either String (SomeContext DynamicValue))
              contextDyn = dyn configVar >>= \case
                Nothing                -> pure (Right (SomeContext EmptyContext))
                Just Configuration{..} -> layoutContext coContents

          eViewers <- forM viewers $ \v ->
            buildComplexViewer splicesDyn contextDyn
              [ t | t <- tools, toolTargets viewerTitles (vfTitle v) t ]
              v

          case sequence eViewers of
            Left err -> pure (Left err)
            Right viewerList -> do
              viewersVar   <- newVariable viewerList
              scenariosVar <- newVariable []
              pure (Right (Ensemble setupVar splicesDyn configVar viewersVar scenariosVar))

------------------------------------------------------------
-- Stanza field extraction (total, so no incomplete-pattern risk)
------------------------------------------------------------

data ViewerFields = ViewerFields
  { vfTitle     :: String
  , vfVar       :: String
  , vfSize      :: Dimensions
  , vfCenter    :: Maybe String
  , vfPixelSize :: Maybe String
  , vfIterLimit :: Maybe String
  , vfEscape    :: Maybe String
  , vfVanish    :: Maybe String
  , vfPixelVar  :: Maybe String
  , vfResizable :: Bool
  , vfPosition  :: Dimensions
  , vfCode      :: ParsedCode
  }

viewerFields :: ParsedStanza -> Maybe ViewerFields
viewerFields = \case
  ViewerStanza{..} -> Just ViewerFields
    { vfTitle = vsTitle, vfVar = vsVar, vfSize = vsSize
    , vfCenter = vsCenter, vfPixelSize = vsPixelSize
    , vfIterLimit = vsIterLimit, vfEscape = vsEscape, vfVanish = vsVanish
    , vfPixelVar = vsPixelVar, vfResizable = vsResizable, vfPosition = vsPosition
    , vfCode = vsCode
    }
  _ -> Nothing

data ToolFields = ToolFields
  { tfName     :: String
  , tfFor      :: Maybe String
  , tfShortcut :: String
  , tfIcon     :: Maybe String
  , tfDrawLayer         :: Int
  , tfRefreshOnActivate :: Bool
  , tfRefreshCanUpdate  :: Bool
  , tfConfig   :: [ConfigStmt]
  , tfBlocks   :: [ParsedToolBlock]
  }

toolFields :: ParsedStanza -> Maybe ToolFields
toolFields = \case
  ToolStanza{..} -> Just ToolFields
    { tfName = tsName, tfFor = tsFor, tfShortcut = tsShortcut
    , tfIcon = tsIcon
    , tfDrawLayer = tsDrawLayer
    , tfRefreshOnActivate = tsRefreshOnActivate
    , tfRefreshCanUpdate = tsRefreshCanUpdate
    , tfConfig = tsConfig
    , tfBlocks = tsBlocks
    }
  _ -> Nothing

------------------------------------------------------------
-- Tool target resolution
------------------------------------------------------------

-- | Check up front, before doing any (more expensive) building, that
-- every @\@viewer@ title is unique and every @\@tool@'s @for=@ (or its
-- absence) resolves unambiguously.
checkTitlesAndTargets :: [String] -> [ToolFields] -> Either (SourceRange, String) ()
checkTitlesAndTargets viewerTitles tools = do
  let dupes = viewerTitles \\ nub viewerTitles
  case dupes of
    (d:_) -> Left (NoSourceRange, "Two `@viewer` stanzas share the title `" ++ d ++ "`.")
    []    -> pure ()
  forM_ tools $ \ToolFields{..} -> case tfFor of
    Just title | title `notElem` viewerTitles ->
      Left (NoSourceRange, "Tool `" ++ tfName ++ "` targets an unknown viewer `" ++ title ++ "`.")
    Nothing | length viewerTitles /= 1 ->
      Left (NoSourceRange, "Tool `" ++ tfName ++ "` doesn't specify `for=`, and there " ++
            (if null viewerTitles then "are no viewers" else "is more than one viewer") ++
            " to default to.")
    _ -> pure ()

-- | Whether a tool (already validated by 'checkTitlesAndTargets') targets
-- the named viewer.
toolTargets :: [String] -> String -> ToolFields -> Bool
toolTargets viewerTitles viewerTitle ToolFields{tfFor} = case tfFor of
  Just title -> title == viewerTitle
  Nothing    -> length viewerTitles == 1

------------------------------------------------------------
-- Small construction helpers
------------------------------------------------------------

-- | Build a 'Mapped' value whose computed result is already known,
-- ignoring whatever the source variable holds. Only for fields a widget
-- never writes back to (e.g. a 'CheckBox'\/'Selection'\/'ColorPicker's own
-- variable-name label) -- anything a widget's text box can edit, like a
-- 'TextBox's 'exprValue', needs 'reactiveSomeValue' instead, or edits
-- would silently fail to take effect (see its own comment).
constMapped :: (Typeable src, Typeable a) => src -> a -> IO (Mapped src a)
constMapped srcVal result = newVariable srcVal >>= newMapped (pure (const result))

constParsed :: Typeable a => String -> Either String a -> IO (Parsed a)
constParsed = constMapped

-- | Build a genuinely reactive 'Parsed' 'SomeValue': initialized to the
-- given source text, re-parsed against the given environment every time
-- something writes new text into it. 'TextBox's 'UIVariable' is read via
-- 'layoutToArgs' (both for an ensemble's own @\@config@ panel and for a
-- tool's own config panel) through an 'EventArgument' whose getter re-reads
-- @dyn exprValue@ on every access and whose setter writes straight to
-- @source exprValue@, expecting the usual 'Mapped' re-parse-on-write
-- behavior -- unlike 'constMapped', which would silently freeze the value
-- at whatever it was built with, so an edit in the UI would appear to work
-- (the text box updates) while the value every reader sees never changes.
reactiveSomeValue :: forall env ty. (KnownEnvironment env, KnownType ty)
                   => EnvironmentProxy env -> TypeProxy ty -> String -> IO (Parsed SomeValue)
reactiveSomeValue env ty initial = do
  let f src = bimap (`ppFullError` src) (SomeValue env ty) (parseValue @env @ty Map.empty src)
  newVariable initial >>= newMapped (pure f)

parseConstantE :: TypeProxy ty -> String -> Either (SourceRange, String) (HaskellType ty)
parseConstantE ty s = left (errorLocation &&& unlines . pp) (parseConstant ty s)

parseMaybeAttrValue :: Maybe String -> Either (SourceRange, String) (Maybe ParsedValue)
parseMaybeAttrValue Nothing = Right Nothing
parseMaybeAttrValue (Just s) =
  Just <$> left (errorLocation &&& unlines . pp) (parseParsedValue Map.empty s)

-- | Chain a pure validation step into an IO-returning continuation,
-- short-circuiting on 'Left'.
(>>=?) :: Either e a -> (a -> IO (Either e b)) -> IO (Either e b)
ea >>=? f = either (pure . Left) f ea
infixl 1 >>=?

------------------------------------------------------------
-- Configuration\/setup -> Layout
------------------------------------------------------------

configStmtToLayout :: ConfigStmt -> IO (Either (SourceRange, String) Layout)
configStmtToLayout = \case
  ConfigText s   -> Right . PlainText <$> newVariable s
  ConfigButton s -> Right . Button <$> newVariable s
  ConfigDecl{..} -> buildConfigDecl cdRange cdName cdType cdDefault cdLabel cdChoices cdUsing
  ConfigRow stmts -> do
    results <- mapM configStmtToLayout stmts
    case sequence results of
      Left err      -> pure (Left err)
      Right layouts -> Right . Horizontal <$> newVariable layouts

-- | The names of every variable a config\/setup\/tool statement declares,
-- recursing into @\@row@ blocks (used for 'Actor.Tool.toolVars').
configStmtNames :: ConfigStmt -> [String]
configStmtNames = \case
  ConfigDecl{cdName} -> [cdName]
  ConfigRow stmts    -> concatMap configStmtNames stmts
  ConfigText{}       -> []
  ConfigButton{}     -> []

buildConfigDecl :: SourceRange -> String -> SomeType -> ParsedValue -> Maybe String -> Maybe [String]
                -> Maybe [(String, FSType)]
                -> IO (Either (SourceRange, String) Layout)
buildConfigDecl sr name (SomeType ty) defaultValue mlabel mchoices mUsing = withKnownType ty $
  let lbl = Label (fromMaybe name mlabel)

      evalDefault = case atType @'[] defaultValue ty of
        TC (Left err) -> Left (sr, "The default value for `" ++ name ++
                                    "` doesn't match its declared type: " ++ ppError err)
        TC (Right v)  -> Right (evaluateInContext EmptyContext v)

      -- A formula field (`using z:C, C:C <- ...`), regardless of its
      -- declared type -- a boolean stopping condition or a color rule is
      -- just as legitimate a formula as a numeric one. Its default-value
      -- expression is typechecked against the `using` environment instead
      -- of the empty one, and the resulting Value AST (with its free
      -- variables left unresolved) is what gets stored under this
      -- declaration's name in the splices map built by 'Actor.Script.
      -- parseScript', for `@viewer`/`@tool` bodies to reference via
      -- `«name»`. Unlike an ordinary field, editing this one at runtime
      -- couldn't do anything useful: every splice reference to it was
      -- already resolved once, at parse time, into whatever code spliced
      -- it in -- so this is intentionally not wired up like
      -- 'reactiveSomeValue' below.
      buildFormulaField envSpec =
        withEnvFromMap (Map.fromList [ (n, withType t SomeType) | (n, t) <- envSpec ]) $
          \(env' :: EnvironmentProxy env') -> withEnvironment env' $
            case atType @env' defaultValue ty of
              TC (Left err) -> pure (Left (sr, "The formula for `" ++ name ++
                                                "` doesn't typecheck: " ++ ppError err))
              TC (Right v) -> do
                labelV     <- newVariable lbl
                exprNameP  <- constParsed name (Right name)
                exprTypeP  <- constParsed (ppType ty) (Right (SomeType ty))
                exprEnvV   <- newVariable (SomeEnvironment env')
                exprValueP <- constParsed (name ++ " (defined in script)")
                                           (Right (SomeValue env' ty v))
                pure (Right (TextBox labelV (UIVariable exprNameP exprTypeP exprEnvV exprValueP)))

  in case mUsing of
    Just envSpec -> buildFormulaField envSpec
    Nothing -> case (ty, mchoices) of

      (BooleanType, _) -> evalDefault >>=? \v -> do
        labelV <- newVariable lbl
        varP   <- constParsed name (Right name)
        valueV <- newVariable v
        pure (Right (CheckBox labelV varP valueV))

      (ColorType, _) -> evalDefault >>=? \v -> do
        labelV <- newVariable lbl
        varP   <- constParsed name (Right name)
        valueP <- constParsed name (Right v)
        pure (Right (ColorPicker labelV varP valueP))

      (IntegerType, Just choices) -> evalDefault >>=? \ix ->
        if ix < 0 || ix >= fromIntegral (length choices)
        then pure (Left (sr, "The choice index " ++ show ix ++ " for `" ++ name ++
                              "` is out of range (there " ++
                              (if length choices == 1 then "is 1 choice" else
                               "are " ++ show (length choices) ++ " choices") ++ ")."))
        else do
          labelV    <- newVariable lbl
          varP      <- constParsed name (Right name)
          ixV       <- newVariable ix
          choicesV  <- newVariable choices
          pure (Right (Selection labelV varP ixV choicesV))

      (_, _) -> evalDefault >>=? \v -> do
        labelV     <- newVariable lbl
        exprNameP  <- constParsed name (Right name)
        exprTypeP  <- constParsed (ppType ty) (Right (SomeType ty))
        exprEnvV   <- newVariable (SomeEnvironment EmptyEnvProxy)
        exprValueP <- reactiveSomeValue EmptyEnvProxy ty (showValue ty v)
        pure (Right (TextBox labelV (UIVariable exprNameP exprTypeP exprEnvV exprValueP)))

-- | One @\@setup@\/@\@config@ stanza's statements, combined into a single
-- titled 'Panel'.
buildStanzaPanel :: (String, [ConfigStmt]) -> IO (Either (SourceRange, String) Layout)
buildStanzaPanel (title, stmts) = do
  results <- mapM configStmtToLayout stmts
  case sequence results of
    Left err -> pure (Left err)
    Right layouts -> do
      bodyLayout <- Vertical <$> newVariable layouts
      titleV     <- newVariable (Label title)
      bodyV      <- newVariable bodyLayout
      pure (Right (Panel titleV bodyV))

-- | All of a project's @\@setup@ (or @\@config@) stanzas, combined into
-- one 'Configuration'. 'Nothing' when there are no such stanzas at all
-- (so the ensemble ends up with no setup\/configuration panel, same as a
-- classic YAML project with no @setup:@\/@configuration:@ key).
buildMaybeConfiguration :: String -> [(String, [ConfigStmt])]
                         -> IO (Either (SourceRange, String) (Maybe Configuration))
buildMaybeConfiguration _ [] = pure (Right Nothing)
buildMaybeConfiguration overallTitle stanzaGroups = do
  panelResults <- mapM buildStanzaPanel stanzaGroups
  case sequence panelResults of
    Left err -> pure (Left err)
    Right panels -> do
      titleP   <- constParsed overallTitle (Right overallTitle)
      sizeV    <- newVariable (Dimensions (400, 400))
      contents <- Vertical <$> newVariable panels
      pure (Right (Just (Configuration titleP sizeV contents)))

------------------------------------------------------------
-- Viewer -> ComplexViewer
------------------------------------------------------------

buildComplexViewer :: Dynamic (Either String Splices)
                    -> Dynamic (Either String (SomeContext DynamicValue))
                    -> [ToolFields]
                    -> ViewerFields
                    -> IO (Either (SourceRange, String) ComplexViewer)
buildComplexViewer splicesDyn contextDyn myTools ViewerFields{..} =
  parseConstantE ComplexType (fromMaybe "0" vfCenter) >>=? \centerVal ->
  parseConstantE RealType (fromMaybe "1/128" vfPixelSize) >>=? \pixelVal ->
  parseMaybeAttrValue vfIterLimit >>=? \mIterPV ->
  parseMaybeAttrValue vfEscape >>=? \mEscPV ->
  parseMaybeAttrValue vfVanish >>=? \mVanPV -> do

    cvTitleP     <- constParsed vfTitle (Right vfTitle)
    cvSizeV      <- newVariable vfSize
    cvPositionV  <- newVariable vfPosition
    cvCanResizeV <- newVariable vfResizable
    cvCenterP    <- constParsed (fromMaybe "0" vfCenter) (Right centerVal)
    cvPixelSizeP <- constParsed (fromMaybe "1/128" vfPixelSize) (Right pixelVal)
    cvCoordP     <- constParsed vfVar (Right vfVar)
    cvPixelM     <- constMapped vfPixelVar (Right vfPixelVar)
    cvEscapeP    <- constParsed (fromMaybe "" vfEscape) (Right mEscPV)
    cvVanishP    <- constParsed (fromMaybe "" vfVanish) (Right mVanPV)
    cvIterLimP   <- constParsed (fromMaybe "" vfIterLimit) (Right mIterPV)
    cvPrepV      <- newVariable Nothing

    let runBuild :: Either String (SomeContext DynamicValue)
                 -> Either String Splices
                 -> Either (SourceRange, String) SomeViewerWithContext
        runBuild ectx esplices = case (ectx, esplices) of
          (Left err, _) -> Left (NoSourceRange, err)
          (_, Left err) -> Left (NoSourceRange, err)
          (Right (SomeContext (configCtx :: Context DynamicValue cEnv)), Right splices) ->
            let env = contextToEnv configCtx
            in fromMaybe (Left (NoSourceRange, "INTERNAL ERROR: redefined internal argument")) $
               assertMissingViewerArgs env $ do
                 vc   <- left (NoSourceRange,) (buildViewerContext env configCtx splices vfVar mIterPV mEscPV mVanPV)
                 code <- parseViewerScript vfPixelVar vc vfCode
                 pure (SomeViewerWithContext configCtx Nothing code)

        codeMapper :: Dynamic (CodeString -> Either (SourceRange, String) SomeViewerWithContext)
        codeMapper = (\ectx esplices -> const (runBuild ectx esplices)) <$> contextDyn <*> splicesDyn

    cvCodeSourceV <- newVariable (CodeString "")
    cvCodeM       <- newMapped codeMapper cvCodeSourceV

    let toolLimits :: (ParsedValue, ParsedValue, ParsedValue)
        toolLimits = ( fromMaybe defaultIterLimit mIterPV
                     , fromMaybe defaultMaxRadius mEscPV
                     , fromMaybe defaultMinRadius mVanPV
                     )

    -- Force the viewer's code now, rather than leaving it to whatever
    -- later consumer happens to be the first to read 'cvCode' (typically
    -- the UI, when the project is opened). A deep typecheck error inside
    -- a lazily-deferred 'Mapped' value that nothing has forced yet would
    -- otherwise surface as a silently-empty/broken viewer rather than a
    -- build error here -- exactly the failure mode this project has been
    -- bitten by once before with the migrated-example YAML files (see
    -- CLAUDE_TODO.md's "Post-hoc bug" writeup).
    codeResult <- getDynamic cvCodeM
    case codeResult of
      Left err -> pure (Left err)
      Right _ -> do
        eTools <- mapM (buildTool (ComplexCoordinate vfVar) toolLimits vfPixelVar) myTools
        case sequence eTools of
          Left err -> pure (Left err)
          Right toolList -> do
            cvToolsV <- newVariable toolList
            pure $ Right ComplexViewer
              { cvTitle = cvTitleP
              , cvSize = cvSizeV
              , cvPosition = cvPositionV
              , cvCanResize = cvCanResizeV
              , cvCenter = cvCenterP
              , cvPixelSize = cvPixelSizeP
              , cvCoord = cvCoordP
              , cvPixel = cvPixelM
              , cvEscapeRadius = cvEscapeP
              , cvVanishRadius = cvVanishP
              , cvIterationLimit = cvIterLimP
              , cvPrep = cvPrepV
              , cvCode = cvCodeM
              , cvTools = cvToolsV
              }

------------------------------------------------------------
-- Tool -> Tool
------------------------------------------------------------

-- | Build a 'Tool' from its parsed fields. A tool stanza's own leading
-- declarations (if any) become its own configuration panel -- built here
-- by reusing 'buildMaybeConfiguration' exactly as a @\@config@ stanza
-- group would be, and set as the tool's 'toolConfig'. From there, wiring
-- those variables into the tool's own event handlers at runtime is
-- entirely existing, unmodified machinery: 'buildToolContext' (called by
-- the UI when a tool is activated) already combines a non-'Nothing'
-- 'toolConfig' with the ensemble context before running the tool's
-- script, for a classic YAML tool's @configuration:@ key exactly the
-- same way.
buildTool :: Coordinate -> (ParsedValue, ParsedValue, ParsedValue) -> Maybe String -> ToolFields
          -> IO (Either (SourceRange, String) Tool)
buildTool coord limits mpixel ToolFields{..} = do
  eToolConfig <- if null tfConfig
                 then pure (Right Nothing)
                 else buildMaybeConfiguration tfName [(tfName, tfConfig)]
  case eToolConfig of
    Left err -> pure (Left err)
    Right toolConfigValue -> do
      nameP      <- constParsed tfName (Right tfName)
      shortcutV  <- newVariable tfShortcut
      shortHelpV <- newVariable ""
      helpV      <- newVariable ""
      iconV      <- newVariable tfIcon
      let ti = ToolInfo { tiName = nameP, tiShortcut = shortcutV, tiShortHelp = shortHelpV
                         , tiHelp = helpV, tiIcon = iconV }

      layerV             <- newVariable tfDrawLayer
      refreshOnActivateV <- newVariable tfRefreshOnActivate
      refreshCanUpdateV  <- newVariable tfRefreshCanUpdate
      configV            <- newVariable toolConfigValue
      showConfigV        <- newVariable (const (pure ()))
      scriptM            <- constMapped (CodeString "") (mergeToolBlocks tfBlocks coord limits mpixel)
      handlerV           <- newVariable (\_ _ -> Nothing)

      let toolVarNames = Set.fromList (concatMap configStmtNames tfConfig)

      pure $ Right Tool
        { toolInfo = ti
        , toolDrawLayer = layerV
        , toolRefreshOnActivate = refreshOnActivateV
        , toolRefreshCanUpdate = refreshCanUpdateV
        , toolConfig = configV
        , toolShowConfig = showConfigV
        , toolScript = scriptM
        , toolEventHandler = handlerV
        , toolVars = pure toolVarNames
        }

data HaskellValueOrError :: Symbol -> FSType -> Exp Type
type instance Eval (HaskellValueOrError name t) = Either String (HaskellType t)

runEnsembleFromSetup :: Backend -> UI -> Ensemble -> IO ()
runEnsembleFromSetup backend UI{..} Ensemble{..} = do

  -- Get a handle for the ensemble
  project <- newEnsemble
  let rerunSetup = pure ()
  let runPostSetup = runEnsemblePostSetup project backend makeLayout makeViewer rerunSetup Ensemble{..}

  -- Run the setup panel
  getDynamic ensembleSetup >>= \case
    Nothing -> runPostSetup
    Just Configuration{..} -> do
      title <- either (throwIO . BadEnsemble) pure =<< getDynamic coTitle
      runSetup project title coContents runPostSetup

runEnsemble :: Backend -> UI -> Ensemble -> IO ()
runEnsemble backend UI{..} Ensemble{..} = do

  -- Get a handle for the ensemble
  project <- newEnsemble
  let rerunSetup = pure ()
  runEnsemblePostSetup project backend makeLayout makeViewer rerunSetup Ensemble{..}


runEnsemblePostSetup :: forall ensembleHandle
                      . ensembleHandle
                     -> Backend
                     -> (ensembleHandle -> String -> Layout -> IO (IO ()))
                     -> (ensembleHandle -> IO () -> Dynamic (Either String SomeEnvironment) -> SomeContext EventArgument_ -> IO () -> IO () -> Viewer -> IO (IO ()))
                     -> IO ()
                     -> Ensemble
                     -> IO ()
runEnsemblePostSetup project backend mkLayout mkViewer rerunSetup Ensemble{..} = do

  -- Make the configuration panel
  (someContext, configArgs, showConfig) <- getDynamic ensembleConfiguration >>= \case
    Nothing -> pure (SomeContext EmptyContext, SomeContext EmptyContext, pure ())
    Just Configuration{..} -> do
      title <- fromRight "???" <$> getDynamic coTitle
      showConfig <- mkLayout project title coContents
      layoutContext' coContents >>= \case
        Left err -> throwIO (BadEnsemble err)
        Right c  -> layoutToArgs coContents >>= \case
          SomeContext' (Left err)   -> throwIO (BadEnsemble err)
          SomeContext' (Right args) -> pure (c, args, showConfig)

  let env = maybe (pure $ Right $ SomeEnvironment EmptyEnvProxy) (layoutEnv . coContents) =<< dyn ensembleConfiguration
  watchDynamic env $ \env' -> putStrLn ("ENV: " ++ show env')
  let mkViewer' x1 x2 = mkViewer x1 x2 env

  -- Make the viewers
  viewers <- getDynamic ensembleViewers
  forM_ viewers $ makeComplexViewer project backend mkViewer' someContext configArgs showConfig rerunSetup

makeComplexViewer :: forall ensembleHandle
                   . ensembleHandle
                  -> Backend
                  -> (ensembleHandle -> IO () -> SomeContext EventArgument_ -> IO () -> IO () -> Viewer -> IO (IO ()))
                  -> SomeContext DynamicValue'
                  -> SomeContext EventArgument_
                  -> IO ()
                  -> IO ()
                  -> ComplexViewer
                  -> IO ()
makeComplexViewer project backend mkViewer someContext configArgs showConfig rerunSetup ComplexViewer{..} = do
   rebuildScript
  where
   rebuildScript = do

    let throwLeft :: IO (Either String a) -> IO a
        throwLeft action = action >>= \case
          Left err -> throwIO (BadEnsemble $ show err)
          Right x  -> pure x

    coord <- fromRight "?coord" <$> getDynamic cvCoord
    let vTitle    = source cvTitle
        vSize     = cvSize
        vPosition = cvPosition

    let scriptCode = cvCode <&> right (\(SomeViewerWithContext _ _ c) -> SomeCode c)
    scriptName <- newMapped (pure $ \n -> if null n then Left "Script title must be non-empty" else Right n)
                  vTitle
    scriptEnv <- newVariable (SomeEnvironment endOfDecls)
    let vScript = UIScript{..}

    vCanResize <- getDynamic cvCanResize
    x0 :+ y0 <- throwLeft (getDynamic cvCenter)
    vCenter <- newVariable (x0, y0)
    vPixelSize <- throwLeft (getDynamic cvPixelSize) >>= newVariable
    let vSaveView = pure ()

    (vDrawCmds, vDrawCmdsChanged, vDrawSink) <- makeToolRunnerForLayer (bToolRunnerFactory backend)

    let vBuildToolHandler = runTool (bToolRunner backend) vDrawSink

    getDynamic cvCode >>= \case

      Left (_, err) -> do
        let vTools = dyn cvTools
            vCodeWithArgs = CodeWithArgs (pure $ Right EmptyContext) Nothing (pure allGrey)
            vListen = const (pure $ pure ())

        putStrLn ("Can't build viewer: " ++ err)
        void $ mkViewer project showConfig configArgs rerunSetup rebuildScript Viewer{..}

      Right (SomeViewerWithContext context mprep code) -> do

        let env = contextToEnv context
            prepUsedVars = case mprep of
              Nothing -> Set.empty
              Just (PrepScript _ prepCode) -> execState (usedVarsInCode prepCode) Set.empty
            usedVars = Set.delete coord (execState (usedVarsInCode code) Set.empty `Set.union` prepUsedVars)

            vListen :: IO () -> IO (IO ())
            vListen = \action -> do
              let changed :: forall env. Context DynamicValue' env -> Dynamic ()
                  changed = \case
                    Bind n _t v ctx'
                      | symbolVal n `Set.member` usedVars
                        -> ((<>) <$> (const () <$> v) <*> changed ctx')
                      | otherwise -> changed ctx'
                    EmptyContext  -> pure ()
              let cc = (\(SomeContext c) -> changed c) someContext
              watchDynamic cc (\_ -> action)

        withEnvironment env $ do

          let vGetArgs = case someContext of
                SomeContext context' -> case sameEnvironment (contextToEnv context) (contextToEnv context') of
                  Just Refl -> do
                    rawResult <- mapContextM @DynamicValue' @HaskellValueOrError (\_ _ -> getDynamic) context'
                    case mapContextM (\_ _ -> id) rawResult of
                      Left err -> pure $
                        Left ("Viewer paused until this issue with the configuration is addressed:\n" ++
                              err)
                      r -> pure r
                  Nothing ->
                    -- Env has prep output vars appended beyond the config env.
                    -- Config vars: read live from someContext (which uses layoutContext').
                    -- Prep output vars: not in someContext, so fall back to snapshot default.
                    let liveEnv = contextToEnv context'
                    in do
                      result <- mapContextM @DynamicValue @HaskellValueOrError
                        (\name ty val ->
                          case lookupEnv name ty liveEnv of
                            Found pf -> withKnownType ty $ getDynamic (getBinding context' pf)
                            _        -> Right <$> getDynamic val)
                        context
                      pure (mapContextM (\_ _ -> id) result)

          withSelectTool <- if coord `Map.member` envToMap env then (:) <$> makeSelectTool coord else pure id
          let vTools = withSelectTool <$> dyn cvTools

          withCompiledViewer (bViewerCompiler backend) mprep code $ \fun -> do
            let vCodeWithArgs = CodeWithArgs vGetArgs (Just code) (pure fun)
            -- FIXME, we should grab the "close this window" action and do something with it
            void $ mkViewer project showConfig configArgs rerunSetup rebuildScript Viewer{..}

layoutToArgs :: Layout -> IO (SomeContext' EventArgument_)
layoutToArgs = \case
  Vertical   xs -> getDynamic xs >>= mapM layoutToArgs <&> mconcat
  Horizontal xs -> getDynamic xs >>= mapM layoutToArgs <&> mconcat
  Panel _ lo    -> getDynamic lo >>= layoutToArgs
  Tabbed ts     -> getDynamic ts >>= mapM (getDynamic . dyn . tiBody >=> layoutToArgs) <&> mconcat
  PlainText _   -> pure mempty
  Button _      -> pure mempty
  Selection _ n x _ -> getDynamic (dyn n) <&> \case
    Left err -> SomeContext' (Left err)
    Right name0 -> case someSymbolVal name0 of
      SomeSymbol name ->
        SomeContext' $ Right $ SomeContext $ Bind name IntegerType (mutableVar x) EmptyContext
  CheckBox _ n b -> getDynamic (dyn n) <&> \case
    Left err -> SomeContext' (Left err)
    Right name0 -> case someSymbolVal name0 of
      SomeSymbol name ->
        SomeContext' $ Right $ SomeContext $ Bind name BooleanType (mutableVar b) EmptyContext
  ColorPicker _ n c -> getDynamic (dyn n) <&> \case
    Left err -> SomeContext' (Left err)
    Right name0 -> case someSymbolVal name0 of
      SomeSymbol name ->
        SomeContext' $ Right $ SomeContext $ Bind name ColorType (mutableArg c) EmptyContext
  ScriptBox {} -> pure $ SomeContext' (Left "Tools cannot accept scripts as configuration variables")
  TextBox _ UIVariable{..} -> getDynamic (dyn exprName) >>= \case
    Left err -> pure $ SomeContext' (Left err)
    Right name0 -> case someSymbolVal name0 of
      SomeSymbol name -> getDynamic (dyn exprValue) <&> \case
        Left err -> SomeContext' (Left err)
        Right (SomeValue env (ty :: TypeProxy ty) _) -> case env of
          BindingProxy{} -> SomeContext' (Left "Tool configuration variables must have empty environments")
          EmptyEnvProxy  -> withKnownType ty $
            let arg :: EventArgument ty
                arg = EventArgument getter setter
                getter = getDynamic (dyn exprValue) <&> \case
                  Left _ -> Nothing
                  Right (SomeValue env' ty' v') -> case env' of
                    BindingProxy{} -> Nothing
                    EmptyEnvProxy -> case sameHaskellType ty ty' of
                      Nothing -> Nothing
                      Just Refl -> Just (evaluate v' EmptyContext)
                setter = Just (\t -> setValue' (source exprValue) . toUserString t)
            in SomeContext' (Right $ SomeContext $ Bind name ty arg EmptyContext)



  -- | A standard tool for selecting the clicked point
makeSelectTool :: String -> IO Tool
makeSelectTool coord = do
  let dep1 = pure (Right noSplices)
      dep2 = pure (Right (ComplexCoordinate coord))
      dep3 = pure (Right (defaultIterLimit, defaultMaxRadius, defaultMinRadius))
      dep4 = pure (Right Nothing)
  tool <- deserializeWith (dep1, dep2, dep3, dep4) (selectToolJson coord)
  either (error . ("Internal error: " ++)) pure tool

selectToolJson :: String -> ByteString
selectToolJson n = "{ \"name\": \"Select " <> UTF8.fromString n <> "\", \"shortcut\": \"S\", \"script\": \"on click-or-drag:\\n  pass\\n\" }"

$(includeFileInSource "../examples/templates/simple-complex-dynamics.yaml"
  "simpleComplexDynamicsTemplate")

$(includeFileInSource "../examples/templates/simple-parametric-complex-dynamics.yaml"
  "simpleParametricComplexDynamicsTemplate")

$(includeFileInSource "../examples/templates/one-variable-complex-dynamics.yaml"
  "complexDynamicsTemplate")

$(includeFileInSource "../examples/templates/one-variable-parametric-complex-dynamics.yaml"
  "parametricComplexDynamicsTemplate")

allTemplates :: [(String, IO Ensemble)]
allTemplates =
  [ template "Simplified complex dynamics" simpleComplexDynamicsTemplate
  , template "Simplified parametric complex dynamics" simpleParametricComplexDynamicsTemplate
  , template "General complex dynamics" complexDynamicsTemplate
  , template "General parametric complex dynamics" parametricComplexDynamicsTemplate
  ]
 where
   template name bs = (name,) $
     either (error . ("Internal error when parsing a template: " ++)) pure =<< deserializeYAML bs

