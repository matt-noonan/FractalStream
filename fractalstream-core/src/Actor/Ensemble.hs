{-# language OverloadedStrings, TemplateHaskell #-}
module Actor.Ensemble
  ( Ensemble(..)
  , NamedScenario(..)
  , parseEnsembleFromFile
  , buildEnsembleFromScript
  , checkScript
  , encodeEnsemble
  , formatScriptError
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
import Language.Value.Typecheck
  (InternalVanishingRadius, InternalEscapeRadius,
   InternalIterations, InternalStuck, InternalIterationLimit)
import Language.Typecheck (TC(..), ppError)
import Language.Code (Code, letInEnv, checkDefiniteAssignment)
import Language.Code.Parser (Splices(..), noSplices, ParsedCode)
import Language.Parser.SourceRange
import Data.Color (black)

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
  , ensembleSourceScript  :: Variable (Maybe String)
    -- ^ The raw text of this ensemble's unified @script:@, if it was
    -- built from one (via 'buildEnsembleFromScript') rather than the
    -- classic multi-key format. A frontend's save path uses this to
    -- write the script straight back out, verbatim, instead of
    -- re-encoding through the classic 'Codec' instance below -- which
    -- would otherwise silently convert a unified-script project into
    -- the classic format on every save.
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

    sourceScript <-ensembleSourceScript-< newOf (pure (pure Nothing))

    build Ensemble setup splices config viewers scenarios sourceScript

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

-- | Serialize an 'Ensemble' back to project-file bytes. Prefers its own
-- unified-script text, verbatim, under a @script:@ key (alongside a
-- @scenarios:@ key if there are any), when it has one -- i.e. when it
-- was built via 'buildEnsembleFromScript' -- and falls back to the
-- classic multi-key 'Codec' instance otherwise. Every frontend's save
-- path should go through this rather than 'serializeYAML' directly, or
-- opening a unified-script project and saving it would silently
-- convert it into the classic format.
encodeEnsemble :: Ensemble -> IO ByteString
encodeEnsemble ens = getDynamic (ensembleSourceScript ens) >>= \case
  Nothing -> serializeYAML ens
  Just scriptText -> do
    scenarios <- getDynamic (ensembleScenarios ens)
    scenariosKV <- if null scenarios
      then pure []
      else (\v -> [("scenarios", v)]) <$> encodeValue scenarios
    pure $ YAML.encode $ Object $ KM.fromList $
      ("script", String (Text.pack scriptText)) : scenariosKV

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
      Right () -> buildWithValidTargets setupGroups configGroups viewers tools viewerTitles >>= \case
        Left err  -> pure (Left err)
        Right ens -> do
          setValue' (ensembleSourceScript ens) (Just scriptText)
          pure (Right ens)

-- | A pure, allocation-free check of a unified project script: parses
-- it, validates its cross-stanza references (a duplicate @\@viewer@
-- title, a tool's unresolved @for=@ target), validates every
-- @\@setup@\/@\@config@ declaration's default value against its
-- declared type, and type-checks every @\@viewer@'s own code against
-- the environment\/splices its declarations would produce -- all
-- without building any runtime 'Ensemble'. This recovers, purely, the
-- same type errors 'buildEnsembleFromScript' catches: per-declaration
-- type-checking was never actually entangled with allocation in this
-- codebase (every one of 'buildConfigDecl'\/'buildViewerContext'\/
-- 'parseViewerScript's own validation logic is already a plain,
-- non-'IO' function -- allocating a 'Variable' for a checked value is
-- always a separate step after it, done by 'buildConfigDecl' and
-- 'buildComplexViewer' themselves, never inside the checking logic
-- itself); this function just calls that existing pure logic directly
-- instead of going through the 'Layout'\/'Ensemble' machinery built
-- around it, which is what actually requires 'IO'.
--
-- Also checks every @\@tool@'s own code, against the ensemble's
-- @\@config@ environment combined with that tool's own declarations
-- (mirroring 'Actor.Event.buildHandlerWith's own
-- 'Actor.Event.runParsedToolScript' call, also already pure) -- this
-- is *more* than 'buildEnsembleFromScript' itself catches at build
-- time, since 'buildTool' stores a tool's
-- 'Actor.Event.ParsedToolScript' unapplied and only runs it against a
-- real environment when the tool is actually activated. So a tool
-- error caught here is new coverage, not something
-- 'buildEnsembleFromScript' would also reject today -- the drift-guard
-- test only compares the two when both agree, not full parity.
checkScript :: String -> Either (SourceRange, String) ()
checkScript scriptText = do
  stanzas <- parseScript scriptText
  let setupGroups  = [ (t, b) | SetupStanza t b <- stanzas ]
      configGroups = [ (t, b) | ConfigStanza t b <- stanzas ]
      viewers      = mapMaybe viewerFields stanzas
      tools        = mapMaybe toolFields stanzas
      viewerTitles = [ vfTitle v | v <- viewers ]

  checkTitlesAndTargets viewerTitles tools

  -- '@setup@ contributes splices only (its values are only ever
  -- spliced into viewer\/tool code as @«name»@, never bound as plain
  -- variables -- 'buildWithValidTargets' wires it the same
  -- asymmetric way: 'ensembleSetup' feeds only this ensemble's
  -- splices, never its context); '@config@ contributes the ordinary-
  -- variable environment only. Mirrors that same split, computed
  -- directly from the parsed declarations' defaults rather than from
  -- an allocated 'Layout's current values (nothing has been edited
  -- yet at check time, so a field's current value is always still
  -- its declared default).
  (_, setupSplices) <- checkConfigGroup setupGroups
  (configEnv, _)     <- checkConfigGroup configGroups
  checkNoDuplicateNames configEnv
  let splices = Splices Map.empty (Map.fromList setupSplices)

  withEnvFromMap (Map.fromList configEnv) $ \env ->
    forM_ viewers $ \v ->
      checkViewer configEnv env splices
        [ t | t <- tools, toolTargets viewerTitles (vfTitle v) t ] v

-- | Mirrors '<#>'s own duplicate-name rejection -- the same check
-- 'Actor.Layout.layoutContext' hits (via its 'SomeContext'' 'Monoid'
-- instance) when combining multiple @\@config@ stanzas' variables
-- into one context, and 'Actor.Ensemble.buildToolContext' hits when
-- combining a tool's own declarations with the ensemble context.
-- 'Map.fromList' alone would just silently let a later entry win,
-- which is *not* what either of those real paths do.
checkNoDuplicateNames :: [(String, SomeType)] -> Either (SourceRange, String) ()
checkNoDuplicateNames entries = case names \\ nub names of
  (d:_) -> Left (NoSourceRange, d ++ " is defined twice.")
  []    -> Right ()
  where names = map fst entries

-- | Validate every declaration in a group of @\@setup@\/@\@config@
-- stanzas, and extract, for each, the ordinary-variable environment
-- entry it contributes (every declaration except a @using@ formula
-- field, which can only ever be referenced as a splice -- see
-- 'buildConfigDecl's own doc comment on 'buildFormulaField') and the
-- splice entry it always contributes. The allocation-free counterpart
-- of 'buildMaybeConfiguration' feeding 'Actor.Layout.layoutContext'\/
-- 'layoutToSplices'.
checkConfigGroup :: [(String, [ConfigStmt])]
                  -> Either (SourceRange, String) ([(String, SomeType)], [(String, ParsedValue)])
checkConfigGroup = checkConfigStmts . concatMap snd

checkConfigStmts :: [ConfigStmt] -> Either (SourceRange, String) ([(String, SomeType)], [(String, ParsedValue)])
checkConfigStmts stmts = do
  parts <- traverse checkConfigStmt stmts
  pure (concatMap fst parts, concatMap snd parts)

checkConfigStmt :: ConfigStmt -> Either (SourceRange, String) ([(String, SomeType)], [(String, ParsedValue)])
checkConfigStmt = \case
  ConfigText{}     -> Right ([], [])
  ConfigButton{}   -> Right ([], [])
  ConfigRow stmts' -> checkConfigStmts stmts'
  ConfigDecl{..}    -> do
    checkConfigDecl cdRange cdName cdType cdDefault cdChoices cdUsing
    case cdUsing of
      Just _  -> pure ([], [(cdName, cdDefault)])
      Nothing -> pure ([(cdName, cdType)], [(cdName, cdDefault)])

-- | The pure, allocation-free counterpart of 'buildConfigDecl': the
-- exact same validation (a plain field's default against its declared
-- type, or a @using@ formula's default against its own environment,
-- or a choice index against its list of choices), minus every
-- subsequent 'newVariable' call -- those build a 'Layout' for display
-- and don't change whether this declaration is valid. Keep this in
-- sync with 'buildConfigDecl' (the test suite cross-checks 'checkScript'
-- and 'buildEnsembleFromScript' reject the same scripts, to catch
-- drift between them).
checkConfigDecl :: SourceRange -> String -> SomeType -> ParsedValue
                -> Maybe [String] -> Maybe [(String, FSType)]
                -> Either (SourceRange, String) ()
checkConfigDecl sr name (SomeType ty) defaultValue mchoices mUsing = withKnownType ty $
  let evalDefault = case atType @'[] defaultValue ty of
        TC (Left err) -> Left (sr, "The default value for `" ++ name ++
                                    "` doesn't match its declared type: " ++ ppError err)
        TC (Right v)  -> Right (evaluateInContext EmptyContext v)

      checkFormulaField envSpec =
        withEnvFromMap (Map.fromList [ (n, withType t SomeType) | (n, t) <- envSpec ]) $
          \(env' :: EnvironmentProxy env') -> withEnvironment env' $
            case atType @env' defaultValue ty of
              TC (Left err) -> Left (sr, "The formula for `" ++ name ++
                                          "` doesn't typecheck: " ++ ppError err)
              TC (Right _)  -> Right ()

  in case mUsing of
       Just envSpec -> checkFormulaField envSpec
       Nothing -> case (ty, mchoices) of
         (IntegerType, Just choices) -> evalDefault >>= \ix ->
           if ix < 0 || ix >= fromIntegral (length choices)
           then Left (sr, "The choice index " ++ show ix ++ " for `" ++ name ++
                           "` is out of range (there " ++
                           (if length choices == 1 then "is 1 choice" else
                            "are " ++ show (length choices) ++ " choices") ++ ").")
           else Right ()
         _ -> evalDefault >> Right ()

-- | The pure, allocation-free counterpart of 'buildComplexViewer's own
-- type-check of a viewer's code (mirroring its internal @runBuild@
-- exactly, down to the @INTERNAL ERROR@ case -- see 'buildComplexViewer'
-- for why it can't happen in practice), then its own targeting tools'
-- code too (mirroring 'buildComplexViewer's own @toolLimits@\/
-- 'ComplexCoordinate' computation, passed to 'buildTool' the same
-- way). The 'Context' passed to 'buildViewerContext' is never actually
-- a real build's allocated one here: that function only ever threads
-- its @vcContext@ argument through to its result, never inspects it
-- (confirmed by reading its definition), so a context built entirely
-- from 'error' placeholders via 'envToContext' is safe -- nothing in
-- this check-only path ever forces it. (If that invariant ever
-- changes, this will fail loudly -- including in the test suite --
-- rather than silently.)
checkViewer :: forall env. [(String, SomeType)] -> EnvironmentProxy env -> Splices
            -> [ToolFields] -> ViewerFields -> Either (SourceRange, String) ()
checkViewer configEnv env splices myTools ViewerFields{..} = do
  _ <- parseConstantE ComplexType (fromMaybe "0" vfCenter)
  _ <- parseConstantE RealType (fromMaybe "1/128" vfPixelSize)
  mIterPV <- parseMaybeAttrValue vfIterLimit
  mEscPV  <- parseMaybeAttrValue vfEscape
  mVanPV  <- parseMaybeAttrValue vfVanish

  -- Checks the main code (and, below, its tools) against whatever
  -- environment they actually see: plain config, or config merged
  -- with a `prepare` block's own declared outputs -- mirroring
  -- 'buildComplexViewer's own 'finishBuild'. Needs its own explicit
  -- type signature, polymorphic in `someEnv` separately from this
  -- function's own `env`, for the same "untouchable type variable"
  -- reason 'checkTool's caller below does: GHC can't otherwise pin
  -- down 'assertMissingViewerArgs'`s rank-n @t@ from context alone.
  let checkMainAndTools :: forall someEnv. EnvironmentProxy someEnv -> Either (SourceRange, String) ()
      checkMainAndTools someEnv =
        fromMaybe (Left (NoSourceRange, "INTERNAL ERROR: redefined internal argument")) $
          assertMissingViewerArgs someEnv $ do
            let dummyCtx :: Context DynamicValue someEnv
                dummyCtx = envToContext someEnv (\_ _ -> error "checkViewer: dummy context value forced unexpectedly")
            vc <- left (NoSourceRange,) (buildViewerContext someEnv dummyCtx splices vfVar mIterPV mEscPV mVanPV)
            _  <- parseViewerScript vfPixelVar vc vfCode
            pure ()

      -- A `prepare` block's own declared outputs merge into the plain
      -- config environment (checked for a name collision with it, the
      -- same way 'buildComplexViewer's real `<#>`-based merge would
      -- reject one at build time) before its code -- and then the
      -- main code -- are checked against the result.
      viewerCheck :: Either (SourceRange, String) ()
      viewerCheck = case vfPrepare of
        Nothing -> checkMainAndTools env
        Just (outputSpecs, prepParsedCode) -> do
          checkNoDuplicateNames (configEnv ++ outputSpecs)
          withEnvFromMap (Map.fromList (configEnv ++ outputSpecs)) $ \combinedEnv -> do
            _ <- checkPrepCode (map fst outputSpecs) combinedEnv prepParsedCode
            checkMainAndTools combinedEnv
  viewerCheck

  let toolLimits = ( fromMaybe defaultIterLimit mIterPV
                    , fromMaybe defaultMaxRadius mEscPV
                    , fromMaybe defaultMinRadius mVanPV
                    )
  forM_ myTools (checkTool configEnv (ComplexCoordinate vfVar) toolLimits vfPixelVar)

-- | The pure, allocation-free counterpart of a tool's own code getting
-- type-checked -- in the real app, this first happens inside
-- 'Actor.Event.buildHandlerWith' (itself already pure past its
-- 'SomeContext' argument), only once the tool is actually activated.
-- Combines the ensemble's own @\@config@ environment with this tool's
-- own declarations (mirroring 'buildToolContext's
-- 'Language.Environment.<#>'-based combining, including its rejection
-- of a name defined on both sides -- see 'checkNoDuplicateNames')
-- before type-checking its merged event-handler blocks against the
-- result, the same way 'mergeToolBlocks' feeds 'buildTool'.
-- 'runParsedToolScript's errors already carry a real 'SourceRange'
-- (threaded through since -- unlike when this was first written --
-- 'ParsedToolBlock'\/'CombinedEventHandler' no longer discard it), so
-- there's no need to tag anything with 'NoSourceRange' here.
checkTool :: [(String, SomeType)] -> Coordinate -> (ParsedValue, ParsedValue, ParsedValue)
          -> Maybe String -> ToolFields -> Either (SourceRange, String) ()
checkTool ensembleEnv coord limits mpixel ToolFields{..} = do
  (toolEnvEntries, _) <- checkConfigStmts tfConfig
  checkNoDuplicateNames (ensembleEnv ++ toolEnvEntries)
  withEnvFromMap (Map.fromList (ensembleEnv ++ toolEnvEntries)) $ \(env :: EnvironmentProxy env) ->
    assertMissingClickArgs env $
    assertMissingDragArgs env $
    assertMissingUnitArgs env $
    case runParsedToolScript (mergeToolBlocks tfBlocks coord limits mpixel) env of
      Left err -> Left err
      Right _  -> Right ()

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
              viewersVar     <- newVariable viewerList
              scenariosVar   <- newVariable []
              sourceScriptVar <- newVariable Nothing
              pure (Right (Ensemble setupVar splicesDyn configVar viewersVar scenariosVar sourceScriptVar))

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
  , vfPrepare   :: Maybe ([(String, SomeType)], ParsedCode)
  , vfCode      :: ParsedCode
  }

viewerFields :: ParsedStanza -> Maybe ViewerFields
viewerFields = \case
  ViewerStanza{..} -> Just ViewerFields
    { vfTitle = vsTitle, vfVar = vsVar, vfSize = vsSize
    , vfCenter = vsCenter, vfPixelSize = vsPixelSize
    , vfIterLimit = vsIterLimit, vfEscape = vsEscape, vfVanish = vsVanish
    , vfPixelVar = vsPixelVar, vfResizable = vsResizable, vfPosition = vsPosition
    , vfPrepare = vsPrepare
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
  -- 'parseInputValue', not 'parseValue': it's what the classic codec's
  -- own 'UIVariable' text-entry field uses, and its special handling of
  -- list-typed input (wrapping bare, unbracketed text in @[...]@ before
  -- parsing) is exactly what 'toUserString's own list rendering expects
  -- on the other end -- 'toUserString' strips the brackets back off a
  -- list value for display, matching this.
  let f src = bimap (`ppFullError` src) (SomeValue env ty) (parseInputValue @env @ty Map.empty src)
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
        exprValueP <- reactiveSomeValue EmptyEnvProxy ty (toUserString ty v)
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

-- | A type-appropriate placeholder value, used only to seed a context
-- entry that a correct script will always overwrite before ever
-- reading it (see 'buildPrepOutputCtx' in 'buildComplexViewer').
zeroHaskellValue :: TypeProxy t -> HaskellType t
zeroHaskellValue = \case
  BooleanType    -> False
  IntegerType    -> 0
  RealType       -> 0
  ComplexType    -> 0
  RationalType   -> (0, 0)
  ColorType      -> black
  VoidType       -> ()
  ImageType      -> 0
  ListType _     -> []
  TextType       -> ""
  PairType x y   -> (zeroHaskellValue x, zeroHaskellValue y)

-- | Declare one more name in an environment, for use in a 'Kleisli'
-- chain (@declareE p1 t1 \<=\< declareE p2 t2 \<=\< ...@) building up a
-- bookkeeping-extended environment one name at a time -- the same
-- pattern 'Actor.Viewer.Complex.ComplexViewer's own codec uses for
-- its preparation script's environment, copied here so a unified-
-- script `prepare ...:` block behaves identically.
declareE :: forall n t e. KnownSymbol n
         => Proxy n -> TypeProxy t -> EnvironmentProxy e
         -> Either (SourceRange, String) (EnvironmentProxy ('(n, t) ': e))
declareE _ t e = case lookupEnv' (Proxy @n) e of
  Found' _ _ -> Left (NoSourceRange, "Internal error: duplicate `" ++ symbolVal (Proxy @n) ++ "`.")
  Absent' pf -> Right (recallIsAbsent pf $ declare t e)

-- | The bookkeeping names a preparation script's own code can see,
-- on top of whatever environment it's declared against: per-pixel
-- position\/step, the stopping-condition state, and the output color
-- slot -- the same ones 'Actor.Viewer.Complex.ComplexViewer's codec
-- declares for the classic YAML @preparation:@ key's own code. The
-- order matters: it's exactly the order 'buildComplexViewer's own
-- @letInEnv@ chain discharges them in.
type PrepBookkeeping e =
  '(InternalIterations, 'IntegerT) ': '(InternalStuck, 'BooleanT) ':
  '(InternalIterationLimit, 'IntegerT) ': '(InternalEscapeRadius, 'RealT) ':
  '(InternalVanishingRadius, 'RealT) ': '(InternalX, 'RealT) ':
  '(InternalY, 'RealT) ': '(InternalDX, 'RealT) ': '(InternalDY, 'RealT) ':
  '("color", 'ColorT) ': e

-- | Declares the same bookkeeping names (per-pixel position\/step, the
-- stopping-condition state, and the output color slot) that the
-- classic YAML @preparation:@ key's own prep code sees -- see
-- 'Actor.Viewer.Complex.ComplexViewer's codec, which this mirrors
-- exactly so a prep script behaves identically either way it's
-- written.
declareBookkeeping :: forall e. EnvironmentProxy e
                   -> Either (SourceRange, String) (EnvironmentProxy (PrepBookkeeping e))
declareBookkeeping e0 =
      declareE (Proxy @InternalIterations)      IntegerType
  <=< declareE (Proxy @InternalStuck)           BooleanType
  <=< declareE (Proxy @InternalIterationLimit)  IntegerType
  <=< declareE (Proxy @InternalEscapeRadius)    RealType
  <=< declareE (Proxy @InternalVanishingRadius) RealType
  <=< declareE (Proxy @InternalX)               RealType
  <=< declareE (Proxy @InternalY)               RealType
  <=< declareE (Proxy @InternalDX)              RealType
  <=< declareE (Proxy @InternalDY)              RealType
  <=< declareE (Proxy @"color")                 ColorType
  $ e0

-- | Type-check a @prepare ...:@ block's own code against its declared
-- output names (already bound, alongside config, in @env@) plus
-- bookkeeping, and check definite assignment of every declared
-- output. Shared between the real build ('buildComplexViewer's
-- 'buildPrepScript', which goes on to actually discharge the
-- bookkeeping with concrete values and construct a 'PrepScript' from
-- the result) and the pure, allocation-free live-editor check
-- ('checkViewer', which only needs to know whether it's valid).
checkPrepCode :: forall someEnv. [String] -> EnvironmentProxy someEnv -> ParsedCode
              -> Either (SourceRange, String)
                   (EnvironmentProxy (PrepBookkeeping someEnv), Code (PrepBookkeeping someEnv))
checkPrepCode outputNames env prepParsedCode = do
  env' <- declareBookkeeping env
  prepCode0 <- left (errorLocation &&& unlines . pp) (runParsedBody env' prepParsedCode)
  left (NoSourceRange,) (checkDefiniteAssignment (Set.fromList outputNames) prepCode0)
  pure (env', prepCode0)

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

    let -- Shared tail end of building a viewer, once its context (plain
        -- config, or config merged with preparation-script outputs) is
        -- in hand. Builds the real 'ViewerContext' first -- needed both
        -- for the main code and, if there's a `prepare` block, to
        -- supply that block's own concrete iteration-limit\/escape\/
        -- vanishing-radius values, exactly mirroring the classic YAML
        -- `preparation:` key's own codec, which builds its
        -- 'ViewerContext' before its prep code for the same reason.
        finishBuild :: forall someEnv prepOutputEnv. KnownEnvironment someEnv
                     => Maybe (EnvironmentProxy prepOutputEnv, [String], ParsedCode)
                     -> Context DynamicValue someEnv -> Splices
                     -> Either (SourceRange, String) SomeViewerWithContext
        finishBuild mprepInfo ctx splices =
          let env = contextToEnv ctx
          in fromMaybe (Left (NoSourceRange, "INTERNAL ERROR: redefined internal argument")) $
             assertMissingViewerArgs env $ do
               vc <- left (NoSourceRange,) (buildViewerContext env ctx splices vfVar mIterPV mEscPV mVanPV)
               mprep <- case mprepInfo of
                 Nothing -> pure Nothing
                 Just (prepOutputEnvProxy, outputNames, prepParsedCode) ->
                   Just <$> buildPrepScript prepOutputEnvProxy outputNames ctx vc prepParsedCode
               code <- parseViewerScript vfPixelVar vc vfCode
               pure (SomeViewerWithContext ctx mprep code)

        -- A constant context entry per declared `prepare` output,
        -- holding a type-appropriate placeholder value -- never
        -- actually observed by a correct prep script, since
        -- 'checkDefiniteAssignment' below rejects any script that
        -- could read an output before assigning it. It's only ever
        -- genuinely read as the pure\/interpreter backend's initial
        -- per-pixel seed value, immediately overwritten by the prep
        -- script's own first assignment (see 'Backend.Pure.
        -- interpretViewer').
        buildPrepOutputCtx :: [(String, SomeType)] -> Either String (SomeContext DynamicValue)
        buildPrepOutputCtx = foldM addOne (SomeContext EmptyContext)
          where
            addOne :: SomeContext DynamicValue -> (String, SomeType) -> Either String (SomeContext DynamicValue)
            addOne (SomeContext acc) (name, SomeType ty) = withKnownType ty $
              case someSymbolVal name of
                SomeSymbol nameProxy ->
                  case acc <#> Bind nameProxy ty (pure (zeroHaskellValue ty)) EmptyContext of
                    Left err -> Left err
                    Right merged -> Right (withEnvironment (contextToEnv merged) (SomeContext merged))

        -- Discharges the bookkeeping names 'checkPrepCode' type-checked
        -- against with concrete values -- using `vc`'s own already-
        -- type-checked iteration-limit\/escape\/vanishing-radius, the
        -- same values the main viewer code's bookkeeping uses, not the
        -- raw, unchecked `mIterPV`\/`mEscPV`\/`mVanPV`. The rest
        -- (InternalX\/Y\/DX\/DY\/"color") stay open, supplied per-pixel
        -- by the backend, matching 'PrepScript's own contract.
        buildPrepScript :: forall someEnv prepOutputEnv. KnownEnvironment someEnv
                         => EnvironmentProxy prepOutputEnv -> [String]
                         -> Context DynamicValue someEnv -> ViewerContext someEnv -> ParsedCode
                         -> Either (SourceRange, String) (PrepScript someEnv)
        buildPrepScript prepOutputEnvProxy outputNames ctx vc prepParsedCode = do
          (env', prepCode0) <- checkPrepCode outputNames (contextToEnv ctx) prepParsedCode
          let (_, prepCode) = (env', prepCode0)
                & letInEnv (Const (Scalar typeProxy 0))
                & letInEnv (Const (Scalar typeProxy False))
                & letInEnv (fromMaybe (Const (Scalar typeProxy 100)) (vcIterLimit vc))
                & letInEnv (fromMaybe (Const (Scalar typeProxy 10.0)) (vcEscapes vc))
                & letInEnv (fromMaybe (Const (Scalar typeProxy 0.0001)) (vcVanishes vc))
          withEnvironment prepOutputEnvProxy $ pure (PrepScript prepOutputEnvProxy prepCode)

        runBuild :: Either String (SomeContext DynamicValue)
                 -> Either String Splices
                 -> Either (SourceRange, String) SomeViewerWithContext
        runBuild ectx esplices = case (ectx, esplices) of
          (Left err, _) -> Left (NoSourceRange, err)
          (_, Left err) -> Left (NoSourceRange, err)
          (Right (SomeContext (configCtx :: Context DynamicValue cEnv)), Right splices) ->
            case vfPrepare of
              Nothing -> finishBuild Nothing configCtx splices
              Just (outputSpecs, prepParsedCode) ->
                case buildPrepOutputCtx outputSpecs of
                  Left err -> Left (NoSourceRange, err)
                  Right (SomeContext (prepCtx :: Context DynamicValue prepOutputEnv)) ->
                    case configCtx <#> prepCtx of
                      Left err -> Left (NoSourceRange, err)
                      Right (combinedCtx :: Context DynamicValue combinedEnv) ->
                        withEnvironment (contextToEnv combinedCtx) $
                          finishBuild (Just (contextToEnv prepCtx, map fst outputSpecs, prepParsedCode))
                            combinedCtx splices

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

