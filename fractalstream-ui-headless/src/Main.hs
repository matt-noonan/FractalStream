{- |
Module       : Main
Description  : Headless FractalStream frontend

A frontend analogous to fractalstream-ui-wx: it implements the same
'Actor.UI.UI' interface that drives 'Actor.Ensemble.runEnsemble', but with
no actual GUI. Two modes:

  fractalstream-headless <project.yaml> --snapshot <viewer-name> --output <file.png>
  fractalstream-headless <project.yaml> --scenario <scenario-file>

Snapshot mode renders one named viewer's base per-pixel script to a PNG
and exits. Scenario mode drives a project through a sequence of commands
(see @SCENARIO_LANGUAGE.md@, "Scenario", and "Interpreter") -- selecting
viewers and tools, injecting synthetic events, changing configuration and
the viewport, and capturing or asserting on what got rendered.
-}
module Main where

import Actor.Ensemble (Ensemble, parseEnsembleFromFile, runEnsemble)
import Actor.UI (UI(..))
import Actor.Viewer (Backend, Viewer(..), snapshotToFile)
import Data.DynamicValue (getDynamic)
import Backend (withBackend)
import Scenario (parseScenario)
import Interpreter (runScenario)

import Data.IORef
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure)

data Mode
  = Snapshot { snapshotViewer :: String, snapshotOutput :: FilePath }
  | RunScenario { scenarioFile :: FilePath }

usage :: String
usage = unlines
  [ "usage:"
  , "  fractalstream-headless <project.yaml> --snapshot <viewer-name> --output <file.png>"
  , "  fractalstream-headless <project.yaml> --scenario <scenario-file>"
  ]

main :: IO ()
main = getArgs >>= \case
  (projectFile : rest) -> case parseMode rest of
    Right mode -> run projectFile mode
    Left err   -> putStrLn err >> putStr usage >> exitFailure
  [] -> putStr usage >> exitFailure

-- | Parse the flags following the project file into a 'Mode'. Hand-rolled
-- rather than pulling in an argument-parsing library, since there are only
-- two mutually exclusive shapes to recognize.
parseMode :: [String] -> Either String Mode
parseMode args = do
  flags <- parseFlags args
  case (lookup "scenario" flags, lookup "snapshot" flags, lookup "output" flags) of
    (Just scenario, Nothing, Nothing) -> Right (RunScenario scenario)
    (Nothing, Just viewer, Just out)  -> Right (Snapshot viewer out)
    (Just _, Just _, _) -> Left "--scenario cannot be combined with --snapshot/--output."
    (Just _, _, Just _) -> Left "--scenario cannot be combined with --snapshot/--output."
    (Nothing, Just _, Nothing) -> Left "--snapshot requires --output as well."
    (Nothing, Nothing, Just _) -> Left "--output requires --snapshot as well."
    (Nothing, Nothing, Nothing) -> Left "specify either --snapshot/--output or --scenario."

parseFlags :: [String] -> Either String [(String, String)]
parseFlags = \case
  [] -> Right []
  ("--snapshot" : v : rest) -> (("snapshot", v) :) <$> parseFlags rest
  ("--output"   : v : rest) -> (("output", v)   :) <$> parseFlags rest
  ("--scenario" : v : rest) -> (("scenario", v) :) <$> parseFlags rest
  (flag@"--snapshot" : []) -> Left ("missing value for " ++ flag)
  (flag@"--output"   : []) -> Left ("missing value for " ++ flag)
  (flag@"--scenario" : []) -> Left ("missing value for " ++ flag)
  (bad : _) -> Left ("unrecognized argument: " ++ bad)

run :: FilePath -> Mode -> IO ()
run projectFile mode = withBackend $ \backend ->
  parseEnsembleFromFile projectFile >>= \case
    Left err -> do
      putStrLn ("Could not load " ++ projectFile ++ ": " ++ err)
      exitFailure
    Right ensemble -> case mode of
      Snapshot viewerName outputFile -> runSnapshot backend ensemble viewerName outputFile
      RunScenario scenarioPath       -> runScenarioFile backend ensemble scenarioPath

runSnapshot :: Backend -> Ensemble -> String -> FilePath -> IO ()
runSnapshot backend ensemble viewerName outputFile = do
  -- 'runEnsemble' calls 'makeViewer' once per viewer declared in the
  -- project; track whether one of them had the requested title (and
  -- what titles we did see, for a useful error message if not).
  found <- newIORef False
  seenTitles <- newIORef []
  runEnsemble backend (snapshotUI viewerName outputFile found seenTitles) ensemble
  readIORef found >>= \case
    True  -> pure ()
    False -> do
      titles <- reverse <$> readIORef seenTitles
      putStrLn ("No viewer named " ++ show viewerName ++ ".")
      putStrLn ("Viewers in this project: " ++ show titles)
      exitFailure

-- | The snapshot-mode 'UI': every callback other than @makeViewer@ is a
-- no-op (there's no setup screen or configuration window to show), and
-- @makeViewer@ renders exactly the one viewer whose title matches, via
-- 'renderViewer'.
snapshotUI :: String -> FilePath -> IORef Bool -> IORef [String] -> UI
snapshotUI viewerName outputFile found seenTitles = UI
  { newEnsemble = pure ()
  , runSetup    = \_ _ _ proceed -> proceed
  , makeLayout  = \_ _ _ -> pure (pure ())
  , makeViewer  = \_ _ _ _ _ _ viewer -> do
      title <- getDynamic (vTitle viewer)
      modifyIORef' seenTitles (title :)
      when (title == viewerName) $ do
        writeIORef found True
        renderViewer viewer outputFile
      pure (pure ())
  }

-- | Render a viewer's current state to @outputFile@.
--
-- This is just 'snapshotToFile': the base per-pixel script, downsampled
-- for antialiasing, with no tool-drawn annotations -- snapshot mode never
-- selects a tool or injects any events, so there's never anything in
-- 'Actor.Viewer.vDrawCmds' to composite. A scenario's own @capture@
-- command is what reaches 'Actor.Viewer.Overlay.compositeDrawLayersToFile'
-- (see "Interpreter").
renderViewer :: Viewer -> FilePath -> IO ()
renderViewer viewer outputFile =
  snapshotToFile viewer True outputFile >>= \case
    Nothing  -> putStrLn ("Wrote " ++ outputFile)
    Just err -> putStrLn ("Error rendering viewer: " ++ err)

-- | Parse a scenario file and run it against the already-loaded project.
runScenarioFile :: Backend -> Ensemble -> FilePath -> IO ()
runScenarioFile backend ensemble scenarioPath = do
  src <- readFile scenarioPath
  case parseScenario src of
    Left (_, err) -> do
      putStrLn ("Could not parse " ++ scenarioPath ++ ": " ++ err)
      exitFailure
    Right cmds -> runScenario backend ensemble cmds >>= \case
      Left err -> do
        putStrLn ("Scenario failed: " ++ err)
        exitFailure
      Right () -> putStrLn (scenarioPath ++ ": all " ++ show (length cmds) ++ " command(s) succeeded.")
