{- |
Module       : Main
Description  : Headless FractalStream frontend

A frontend analogous to fractalstream-ui-wx: it implements the same
'Actor.UI.UI' interface that drives 'Actor.Ensemble.runEnsemble', but with
no actual GUI. Five subcommands run a project:

  fsrun snapshot --project <project.fs> --viewer <title> --output <file.png>
  fsrun commands --project <project.fs> --commands <file> [--capture-to <file.png>] [--record-to <file.mov>]
  fsrun scenario --project <project.fs> --name <name> [--capture-to <file.png>] [--record-to <file.mov>]
  fsrun update-preview --project <project.fs>
  fsrun normalize --project <project.fs>

and one makes a project, from a script for the original FractalStream
("Language.Legacy.Translate"):

  fsrun translate --input <script.txt> --output <project.fs>

(the input can also be a FractalStream 1.0 save file).

Snapshot mode renders one named viewer's base per-pixel script to a PNG
and exits. The other two both drive a project through a sequence of
commands (see @SCENARIO_LANGUAGE.md@, "Scenario", and "Interpreter") --
selecting viewers and tools, injecting synthetic events, changing
configuration and the viewport, and capturing or asserting on what got
rendered -- taken either from a standalone commands file (@fsrun commands@)
or from one of the project file's own top-level @scenarios:@ entries, by
name (@fsrun scenario@). Either can be told to write its one capture
(@--capture-to@) or its one movie (@--record-to@) somewhere other than
where the commands say ('Scenario.applyOverrides').

@fsrun update-preview@ is the one mode that changes the project file: it runs
the project's @metadata: preview-commands:@, which must end up capturing
exactly one viewer with @capture viewer "..." as preview@, and saves that
image as the project's preview. Only those commands can capture a
preview, so no other mode ever writes to the project.

Every failure path reports via 'dieWith': printed to /stderr/ (not
stdout, where every success message goes) and a non-zero exit code --
deliberately, since the alternative bit us once already (see commit
history): errors and ordinary output interleaved on the same stream,
with ordinary output's buffering not necessarily flushed before
'exitFailure' tears the process down, can mean an error that was
genuinely printed is never actually seen.
-}
module Main where

import FractalStream.Quoted (quoted)
import FractalStream.Console (transliterateConsole)
import FractalStream.Prelude

import FractalStream.Metadata (gitTag)
import Actor.Ensemble (Ensemble(..), NamedScenario(..), parseEnsembleFromFile, runEnsemble, encodeEnsemble, checkProjectBytes)
import Language.Legacy.Binary (decodeAncientFS, mightBeAncientFS)
import Language.Legacy.Translate (Translation(..), translateLegacyScript)
import Language.Editor.Normalize (normalize)
import Actor.Metadata (ProjectMetadata(..))
import Actor.UI (UI(..))
import Actor.Viewer (Backend, Viewer(..), snapshotToFile, describeViewerIssues)
import Data.DynamicValue (getDynamic, setValue')
import Backend (withBackend)
import Scenario (parseScenario, Overrides(..), applyOverrides)
import Interpreter (runScenario, runPreviewCommands)
import Language.Parser.SourceRange (Indicated(..))

import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T (lenientDecode)
import System.Directory (renameFile)
import System.FilePath (takeBaseName)

import Options.Applicative
  ( Parser, ParserInfo, customExecParser, prefs, showHelpOnEmpty, info, helper, infoOption
  , hsubparser, command, strOption, option, eitherReader, long, short, metavar, help
  , fullDesc, progDesc, footer )
import Control.Concurrent (setNumCapabilities)
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import System.IO (IOMode(..), withFile, hSetEncoding, utf8, hGetContents', hPutStr, hPutStrLn, stderr)

data Mode
  = Snapshot { snapshotViewer :: String, snapshotOutput :: FilePath }
  | RunCommandsFile { commandsFile :: FilePath, commandsOverrides :: Overrides }
  | RunNamedScenario { scenarioName :: String, scenarioOverrides :: Overrides }
  | UpdatePreview
  | Normalize

-- | Report a failure on stderr and exit non-zero. Every error path in
-- this module goes through this (not a bare @putStrLn@), so errors are
-- never competing with ordinary output on the same stream -- see the
-- module header. (Command-line errors are reported by
-- "Options.Applicative" itself, which also uses stderr and a non-zero
-- exit code; only @--help@ goes to stdout.)
dieWith :: String -> IO a
dieWith msg = hPutStrLn stderr msg >> exitFailure

main :: IO ()
main = do
  transliterateConsole
  customExecParser (prefs showHelpOnEmpty) cliInfo >>= \case
    RunProject{..} -> do
      -- Before the backend starts: the LLVM backend sizes its pool of
      -- per-render scratch arenas by the number of capabilities then.
      forM_ invJobs setNumCapabilities
      run invProject invMode
    Translate{..} -> runTranslate invInput invOutput invTitle

-- | A parsed command line.
data Invocation
  = RunProject
    { invJobs    :: Maybe Int   -- ^ @-j@, if given (otherwise every core, from @-N@)
    , invProject :: FilePath
    , invMode    :: Mode
    }
  | Translate
    { invInput  :: FilePath     -- ^ @-@ for stdin
    , invOutput :: FilePath     -- ^ @-@ for stdout
    , invTitle  :: Maybe String
    }

cliInfo :: ParserInfo Invocation
cliInfo = info (helper <*> versionOption <*> cliParser) $
  fullDesc <>
  progDesc "Run a FractalStream project without a GUI: render a viewer, \
           \run scripted commands against it, or remake its preview." <>
  footer "Run `fsrun COMMAND --help` for help on one command."
  where
    versionOption = infoOption gitTag
      (long "version" <> help "Show the FractalStream version and exit")

-- | Each mode is its own subcommand. Every input is a named option
-- (@--project@ and @--jobs@ in all but @translate@), never a positional
-- argument, so a command line reads the same whatever order its options
-- come in.
cliParser :: Parser Invocation
cliParser = hsubparser $ mconcat
  [ subcommand "snapshot"
      "Render one viewer's base image to a PNG file"
      (Snapshot <$> strOption (long "viewer" <> metavar "TITLE"
                               <> help "The title of the viewer to render")
                <*> strOption (long "output" <> short 'o' <> metavar "FILE.png"
                               <> help "Where to write the image"))
  , subcommand "commands"
      "Run a file of scenario-language commands against the project"
      (RunCommandsFile <$> strOption (long "commands" <> metavar "FILE"
                                      <> help "The commands to run (see SCENARIO_LANGUAGE.md)")
                       <*> overridesOptions)
  , subcommand "scenario"
      "Run one of the project's own scenarios, by name"
      (RunNamedScenario <$> strOption (long "name" <> metavar "NAME"
                                       <> help "The name of an entry in the project's `scenarios:`")
                        <*> overridesOptions)
  , subcommand "update-preview"
      "Remake the project's preview image by running its `metadata: preview-commands:`"
      (pure UpdatePreview)
  , subcommand "normalize"
      "Rewrite the project's script with mathematical symbols (⟵, ℝ, z², ...) for ASCII ones"
      (pure Normalize)
  , command "translate" $ info
      (Translate <$> strOption (long "input" <> short 'i' <> metavar "SCRIPT"
                                <> help "The original-FractalStream script: as text, or a FractalStream 1.0 save file (- for stdin)")
                 <*> strOption (long "output" <> short 'o' <> metavar "PROJECT.fs"
                                <> help "Where to write the project (- for stdout)")
                 <*> optional (strOption (long "title" <> metavar "TITLE"
                                <> help "The project's title (default: the save file's title, or the script file's name)")))
      (progDesc "Translate a script for the original FractalStream into a project")
  ]
  where
    subcommand name desc modeArgs = command name $
      info (RunProject <$> jobsOption <*> projectOption <*> modeArgs) (progDesc desc)
    overridesOptions = Overrides
      <$> optional (strOption (long "capture-to" <> metavar "FILE.png"
            <> help "Write the scenario's one `capture` here instead"))
      <*> optional (strOption (long "record-to" <> metavar "FILE.mov"
            <> help "Write the scenario's one `record` here instead (.mov or .avi)"))
    projectOption = strOption (long "project" <> short 'p' <> metavar "PROJECT.fs" <> help "The project file")
    jobsOption = optional $ option positive
      (long "jobs" <> short 'j' <> metavar "N" <>
       help "Render with N threads (default: one per core). Use -j 1 to make \
            \autocolor groups that aren't locked color the same way every run.")
    positive = eitherReader $ \str -> case readMaybe str of
      Just n | n >= 1 -> Right n
      _ -> Left ("expected a positive number of jobs, not " ++ show str)

-- | Translate an original-FractalStream script into a project. The
-- translation is checked (parsed and type-checked) before it's written;
-- if that fails, it's written anyway, since it's meant to be edited, but
-- the problem is reported and the exit code is non-zero.
runTranslate :: FilePath -> FilePath -> Maybe String -> IO ()
runTranslate input output mtitle = do
  bytes <- if input == "-" then BS.getContents else BS.readFile input
  let what = if input == "-" then "the script" else quoted input
      fileTitle = if input == "-" then "Translated script" else takeBaseName input
  -- a FractalStream 1.0 save file, or the text of a script
  (savedTitle, src) <- if mightBeAncientFS bytes
    then either (\err -> dieWith ("Could not read the script in " ++ what ++ ":\n" ++ err))
                (\(t, b) -> pure (Just t, b)) (decodeAncientFS bytes)
    else pure (Nothing, T.unpack (T.decodeUtf8With T.lenientDecode bytes))
  let title = fromMaybe fileTitle (mtitle <|> savedTitle)
  case translateLegacyScript title src of
    Left err -> dieWith ("Could not translate " ++ what ++ ":\n" ++ err)
    Right Translation{..} -> do
      forM_ translationWarnings (hPutStrLn stderr . ("warning: " ++))
      if output == "-" then putStr translatedProject
        else withFile output WriteMode $ \h -> hSetEncoding h utf8 >> hPutStr h translatedProject
      case checkProjectBytes (T.encodeUtf8 (T.pack translatedProject)) of
        Left err -> dieWith ("The translation doesn't load as it is:\n" ++ err)
        Right () -> when (output /= "-") $ putStrLn ("Wrote " ++ output)

run :: FilePath -> Mode -> IO ()
run projectFile mode = withBackend $ \backend ->
  parseEnsembleFromFile projectFile >>= \case
    Left err -> dieWith ("Could not load " ++ quoted projectFile ++ ":\n" ++ err)
    Right ensemble -> case mode of
      Snapshot viewerName outputFile -> runSnapshot backend ensemble viewerName outputFile
      RunCommandsFile path ovs       -> runCommandsFile backend ensemble ovs path
      RunNamedScenario name ovs      -> runNamedScenario backend ensemble ovs name
      UpdatePreview                  -> runUpdatePreview backend ensemble projectFile
      Normalize                      -> runNormalize ensemble projectFile

runSnapshot :: Backend -> Ensemble -> String -> FilePath -> IO ()
runSnapshot backend ensemble viewerName outputFile = do
  -- 'runEnsemble' calls 'makeViewer' once per viewer declared in the
  -- project; track whether one of them had the requested title, and
  -- (separately) whether rendering it then actually succeeded, for a
  -- useful error message either way.
  found  <- newIORef False
  result <- newIORef Nothing
  seenTitles <- newIORef []
  runEnsemble backend (snapshotUI viewerName outputFile found result seenTitles) ensemble
  readIORef found >>= \case
    False -> do
      titles <- reverse <$> readIORef seenTitles
      dieWith ("No viewer named " ++ show viewerName ++ ".\n" ++
               "Viewers in this project: " ++ show titles)
    True -> readIORef result >>= \case
      Just (Just err) -> dieWith ("Error rendering viewer:\n" ++ err)
      _                -> putStrLn ("Wrote " ++ outputFile)

-- | The snapshot-mode 'UI': every callback other than @makeViewer@ is a
-- no-op (there's no setup screen or configuration window to show), and
-- @makeViewer@ renders exactly the one viewer whose title matches,
-- recording whether that succeeded into @result@ rather than deciding
-- here whether that's a fatal error -- 'runSnapshot' does that once
-- 'runEnsemble' has returned.
snapshotUI :: String -> FilePath -> IORef Bool -> IORef (Maybe (Maybe String)) -> IORef [String] -> UI
snapshotUI viewerName outputFile found result seenTitles = UI
  { newEnsemble = pure ()
  , makeLayout  = \_ _ _ -> pure (pure ())
  , makeViewer  = \_ _ _ _ _ viewer -> do
      title <- getDynamic (vTitle viewer)
      modifyIORef' seenTitles (title :)
      when (title == viewerName) $ do
        writeIORef found True
        err <- renderViewer viewer outputFile
        writeIORef result (Just err)
      pure (pure ())
  }

-- | Render a viewer's current state to @outputFile@, returning an error
-- message if that failed.
--
-- This is the base per-pixel script, downsampled for antialiasing, with
-- no tool-drawn annotations -- snapshot mode never selects a tool or
-- injects any events, so there's never anything in 'Actor.Viewer.vDrawCmds'
-- to composite. A scenario's own @capture@ command is what reaches
-- 'Actor.Viewer.Overlay.compositeDrawLayersToFile' (see "Interpreter").
renderViewer :: Viewer -> FilePath -> IO (Maybe String)
renderViewer viewer outputFile = snapshotToFile viewer True outputFile >>= \case
  Left err     -> pure (Just err)
  Right issues -> do
    forM_ (describeViewerIssues issues) (hPutStrLn stderr . ("warning: " ++))
    pure Nothing

-- | Parse a standalone commands file and run it against the already-loaded
-- project.
runCommandsFile :: Backend -> Ensemble -> Overrides -> FilePath -> IO ()
runCommandsFile backend ensemble ovs path = do
  -- Read as UTF-8, like project files, whatever the locale's encoding.
  src <- withFile path ReadMode $ \h -> hSetEncoding h utf8 >> hGetContents' h
  runCommandsText backend ensemble ovs path src

-- | Find a scenario by name among the project's own top-level
-- @scenarios:@ entries and run it, the same way 'runCommandsFile' runs
-- one read from a standalone file.
runNamedScenario :: Backend -> Ensemble -> Overrides -> String -> IO ()
runNamedScenario backend ensemble ovs name = do
  scenarios <- getDynamic (ensembleScenarios ensemble)
  named <- mapM (\ns -> (, ns) <$> getDynamic (nsName ns)) scenarios
  case lookup name named of
    Nothing -> dieWith ("No scenario named " ++ show name ++ ".\n" ++
                         "Scenarios in this project: " ++ show (map fst named))
    Just ns -> do
      src <- getDynamic (nsCommands ns)
      runCommandsText backend ensemble ovs ("scenario " ++ show name) src

-- | Run the project's preview commands and save the image they capture
-- as its preview. The file is only written once that has all
-- succeeded, and then by writing a new file and renaming it over the
-- old one, so a failure part-way never leaves a damaged project behind.
runUpdatePreview :: Backend -> Ensemble -> FilePath -> IO ()
runUpdatePreview backend ensemble projectFile = do
  md <- getDynamic (ensembleMetadata ensemble)
  case mdPreviewCommands md of
    Nothing -> dieWith (quoted projectFile ++ " has no `metadata: preview-commands:` to make a preview with.")
    Just src -> case parseScenario src of
      Left (sr, err) -> dieWith $
        "Could not parse the preview commands:\n" ++ unlines (pp (Indicated sr src)) ++ err
      Right cmds -> runPreviewCommands backend ensemble cmds >>= \case
        Left err -> dieWith ("Could not make a preview:\n" ++ err)
        Right png -> do
          setValue' (ensembleMetadata ensemble) md { mdPreview = Just png }
          bytes <- encodeEnsemble ensemble
          let tmp = projectFile ++ ".tmp"
          BS.writeFile tmp bytes
          renameFile tmp projectFile
          putStrLn ("Updated the preview in " ++ quoted projectFile)

-- | Rewrite the project's script with the mathematical symbols the
-- language accepts ("Language.Editor.Normalize"), saving it (as
-- 'runUpdatePreview' does) only if that changes anything.
runNormalize :: Ensemble -> FilePath -> IO ()
runNormalize ensemble projectFile = do
  old <- getDynamic (ensembleSourceScript ensemble)
  let new = normalize old
  if new == old
    then putStrLn (quoted projectFile ++ "'s script is already normalized.")
    else do
      setValue' (ensembleSourceScript ensemble) new
      bytes <- encodeEnsemble ensemble
      let tmp = projectFile ++ ".tmp"
      BS.writeFile tmp bytes
      renameFile tmp projectFile
      putStrLn ("Normalized the script in " ++ quoted projectFile)

-- | Parse scenario-language source and run it against the already-loaded
-- project, reporting the outcome the same way regardless of whether the
-- commands came from a standalone file or an embedded named scenario
-- (@label@ is just used in the messages, to say which).
-- Any output paths given in @ovs@ replace the commands' own before
-- anything runs.
runCommandsText :: Backend -> Ensemble -> Overrides -> String -> String -> IO ()
runCommandsText backend ensemble ovs label src = case parseScenario src of
  Left (sr, err) -> dieWith $
    "Could not parse " ++ label ++ ":\n" ++
    unlines (pp (Indicated sr src)) ++
    err
  Right cmds0 -> case applyOverrides ovs cmds0 of
   Left err -> dieWith ("Can't run " ++ label ++ ": " ++ err)
   Right cmds -> runScenario backend ensemble cmds >>= \case
    Left err -> dieWith ("Scenario failed:\n" ++ err)
    Right () -> putStrLn (label ++ ": all " ++ show (length cmds) ++ " command(s) succeeded.")
