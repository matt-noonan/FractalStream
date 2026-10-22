{- |
Module       : Main
Description  : Headless FractalStream frontend -- render one viewer to a file

A frontend analogous to fractalstream-ui-wx: it implements the same
'Actor.UI.UI' interface that drives 'Actor.Ensemble.runEnsemble', but with
no actual GUI. For this first cut, it just renders a named viewer's base
per-pixel script to a PNG file and exits; it does not yet run any tool
event handlers or composite their drawn annotations onto the picture (see
the note on 'renderViewer' below for where that will plug in).
-}
module Main where

import Actor.Ensemble (parseEnsembleFromFile, runEnsemble)
import Actor.UI (UI(..))
import Actor.Viewer (Viewer(..), snapshotToFile)
import Data.DynamicValue (getDynamic)
import Backend (withBackend)

import Data.IORef
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = getArgs >>= \case
  [projectFile, viewerName, outputFile] -> run projectFile viewerName outputFile
  _ -> do
    putStrLn "usage: fractalstream-headless <project.yaml> <viewer-name> <output.png>"
    exitFailure

run :: FilePath -> String -> FilePath -> IO ()
run projectFile viewerName outputFile = withBackend $ \backend ->
  parseEnsembleFromFile projectFile >>= \case
    Left err -> do
      putStrLn ("Could not load " ++ projectFile ++ ": " ++ err)
      exitFailure
    Right ensemble -> do
      -- 'runEnsemble' calls 'makeViewer' once per viewer declared in the
      -- project; track whether one of them had the requested title (and
      -- what titles we did see, for a useful error message if not).
      found <- newIORef False
      seenTitles <- newIORef []
      runEnsemble backend (headlessUI viewerName outputFile found seenTitles) ensemble
      readIORef found >>= \case
        True  -> pure ()
        False -> do
          titles <- reverse <$> readIORef seenTitles
          putStrLn ("No viewer named " ++ show viewerName ++ " in " ++ projectFile ++ ".")
          putStrLn ("Viewers in this project: " ++ show titles)
          exitFailure

-- | The headless 'UI': every callback other than @makeViewer@ is a no-op
-- (there's no setup screen or configuration window to show), and
-- @makeViewer@ renders exactly the one viewer whose title matches, via
-- 'renderViewer'.
headlessUI :: String -> FilePath -> IORef Bool -> IORef [String] -> UI
headlessUI viewerName outputFile found seenTitles = UI
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
-- For now this is just 'snapshotToFile': the base per-pixel script,
-- downsampled for antialiasing, with no tool-drawn annotations. When this
-- frontend grows the ability to inject synthetic 'Actor.Event.Event's
-- (via a tool's dispatcher, which 'Actor.Viewer.vBuildToolHandler' already
-- hands back given a merged tool\/ensemble context -- see
-- 'Actor.Ensemble.buildToolContext' -- it should switch to
-- 'Actor.Viewer.withRenderedViewerBuffer' instead, so it can composite
-- each tool's accumulated 'Actor.Viewer.vDrawCmds' onto the same buffer
-- before encoding, rather than discarding them.
renderViewer :: Viewer -> FilePath -> IO ()
renderViewer viewer outputFile =
  snapshotToFile viewer True outputFile >>= \case
    Nothing  -> putStrLn ("Wrote " ++ outputFile)
    Just err -> putStrLn ("Error rendering viewer: " ++ err)
