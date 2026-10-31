{- |
Module       : Main
Description  : Headless FractalStream frontend

A frontend analogous to fractalstream-ui-wx: it implements the same
'Actor.UI.UI' interface that drives 'Actor.Ensemble.runEnsemble', but with
no actual GUI. Three modes:

  fsrun <project.yaml> --snapshot <viewer-name> --output <file.png>
  fsrun <project.yaml> --commands <commands-file>
  fsrun <project.yaml> --scenario <name>

Snapshot mode renders one named viewer's base per-pixel script to a PNG
and exits. The other two both drive a project through a sequence of
commands (see @SCENARIO_LANGUAGE.md@, "Scenario", and "Interpreter") --
selecting viewers and tools, injecting synthetic events, changing
configuration and the viewport, and capturing or asserting on what got
rendered -- taken either from a standalone commands file (@--commands@)
or from one of the project file's own top-level @scenarios:@ entries, by
name (@--scenario@).

Every failure path reports via 'dieWith': printed to /stderr/ (not
stdout, where every success message goes) and a non-zero exit code --
deliberately, since the alternative bit us once already (see commit
history): errors and ordinary output interleaved on the same stream,
with ordinary output's buffering not necessarily flushed before
'exitFailure' tears the process down, can mean an error that was
genuinely printed is never actually seen.
-}
module Main where

import FractalStream.Prelude

import FractalStream.Metadata (gitTag)
import Actor.Ensemble (Ensemble(..), NamedScenario(..), parseEnsembleFromFile, runEnsemble)
import Actor.UI (UI(..))
import Actor.Viewer (Backend, Viewer(..), snapshotToFile, describeViewerIssues)
import Data.DynamicValue (getDynamic)
import Backend (withBackend)
import Scenario (parseScenario)
import Interpreter (runScenario)
import Language.Parser.SourceRange (Indicated(..))

import Data.IORef
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (IOMode(..), withFile, hSetEncoding, utf8, hGetContents', hPutStrLn, stderr)

data Mode
  = Snapshot { snapshotViewer :: String, snapshotOutput :: FilePath }
  | RunCommandsFile { commandsFile :: FilePath }
  | RunNamedScenario { scenarioName :: String }

usage :: String
usage = unlines
  [ "usage:"
  , "  fsrun <project.yaml> --snapshot <viewer-name> --output <file.png>"
  , "  fsrun <project.yaml> --commands <commands-file>"
  , "  fsrun <project.yaml> --scenario <name>"
  , "  fsrun --version"
  ]

-- | Report a failure on stderr and exit non-zero. Every error path in
-- this module goes through this (not a bare @putStrLn@), so errors are
-- never competing with ordinary output on the same stream -- see the
-- module header.
dieWith :: String -> IO a
dieWith msg = hPutStrLn stderr msg >> exitFailure

main :: IO ()
main = getArgs >>= \case
  ["--version"] -> putStrLn gitTag
  (projectFile : rest) -> case parseMode rest of
    Right mode -> run projectFile mode
    Left err   -> dieWith (err ++ "\n" ++ usage)
  [] -> dieWith usage

-- | Parse the flags following the project file into a 'Mode'. Hand-rolled
-- rather than pulling in an argument-parsing library, since there are
-- only a few mutually exclusive shapes to recognize.
parseMode :: [String] -> Either String Mode
parseMode args = do
  flags <- parseFlags args
  let scenario = lookup "scenario" flags
      commands = lookup "commands" flags
      snapshot = lookup "snapshot" flags
      output   = lookup "output"   flags
  case (scenario, commands, snapshot, output) of
    (Just name, Nothing, Nothing, Nothing)    -> Right (RunNamedScenario name)
    (Nothing, Just file, Nothing, Nothing)    -> Right (RunCommandsFile file)
    (Nothing, Nothing, Just viewer, Just out) -> Right (Snapshot viewer out)
    (Nothing, Nothing, Just _, Nothing)       -> Left "--snapshot requires --output as well."
    (Nothing, Nothing, Nothing, Just _)       -> Left "--output requires --snapshot as well."
    (Nothing, Nothing, Nothing, Nothing)      -> Left "specify one of --snapshot/--output, --commands, or --scenario."
    _ -> Left "--snapshot/--output, --commands, and --scenario are mutually exclusive."

parseFlags :: [String] -> Either String [(String, String)]
parseFlags = \case
  [] -> Right []
  ("--snapshot" : v : rest) -> (("snapshot", v) :) <$> parseFlags rest
  ("--output"   : v : rest) -> (("output", v)   :) <$> parseFlags rest
  ("--commands" : v : rest) -> (("commands", v) :) <$> parseFlags rest
  ("--scenario" : v : rest) -> (("scenario", v) :) <$> parseFlags rest
  (flag@"--snapshot" : []) -> Left ("missing value for " ++ flag)
  (flag@"--output"   : []) -> Left ("missing value for " ++ flag)
  (flag@"--commands" : []) -> Left ("missing value for " ++ flag)
  (flag@"--scenario" : []) -> Left ("missing value for " ++ flag)
  (bad : _) -> Left ("unrecognized argument: " ++ bad)

run :: FilePath -> Mode -> IO ()
run projectFile mode = withBackend $ \backend ->
  parseEnsembleFromFile projectFile >>= \case
    Left err -> dieWith ("Could not load " ++ projectFile ++ ":\n" ++ err)
    Right ensemble -> case mode of
      Snapshot viewerName outputFile -> runSnapshot backend ensemble viewerName outputFile
      RunCommandsFile path           -> runCommandsFile backend ensemble path
      RunNamedScenario name          -> runNamedScenario backend ensemble name

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
runCommandsFile :: Backend -> Ensemble -> FilePath -> IO ()
runCommandsFile backend ensemble path = do
  -- Read as UTF-8, like project files, whatever the locale's encoding.
  src <- withFile path ReadMode $ \h -> hSetEncoding h utf8 >> hGetContents' h
  runCommandsText backend ensemble path src

-- | Find a scenario by name among the project's own top-level
-- @scenarios:@ entries and run it, the same way 'runCommandsFile' runs
-- one read from a standalone file.
runNamedScenario :: Backend -> Ensemble -> String -> IO ()
runNamedScenario backend ensemble name = do
  scenarios <- getDynamic (ensembleScenarios ensemble)
  named <- mapM (\ns -> (, ns) <$> getDynamic (nsName ns)) scenarios
  case lookup name named of
    Nothing -> dieWith ("No scenario named " ++ show name ++ ".\n" ++
                         "Scenarios in this project: " ++ show (map fst named))
    Just ns -> do
      src <- getDynamic (nsCommands ns)
      runCommandsText backend ensemble ("scenario " ++ show name) src

-- | Parse scenario-language source and run it against the already-loaded
-- project, reporting the outcome the same way regardless of whether the
-- commands came from a standalone file or an embedded named scenario
-- (@label@ is just used in the messages, to say which).
runCommandsText :: Backend -> Ensemble -> String -> String -> IO ()
runCommandsText backend ensemble label src = case parseScenario src of
  Left (sr, err) -> dieWith $
    "Could not parse " ++ label ++ ":\n" ++
    unlines (pp (Indicated sr src)) ++
    err
  Right cmds -> runScenario backend ensemble cmds >>= \case
    Left err -> dieWith ("Scenario failed:\n" ++ err)
    Right () -> putStrLn (label ++ ": all " ++ show (length cmds) ++ " command(s) succeeded.")
