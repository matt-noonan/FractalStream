{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A first cut at a headless FractalStream driver: mocks 'Actor.UI.UI's
-- callbacks so 'Actor.Ensemble.runEnsemble' runs for real (real backend,
-- real per-tool typecheck, real merged tool\/ensemble context) with no wx
-- widgets at all. Originally written to track down a bug where a codec
-- error was being silently swallowed and a project would open to nothing
-- visible; kept as the seed of a proper headless frontend (run scripts,
-- inject synthetic 'Event's via the dispatcher 'vBuildToolHandler' hands
-- back, snapshot the result).
--
-- Not wired into any package/build yet -- run via, from
-- fractalstream-backend-pure/:
--
--   echo ':main /path/to/project1.fs /path/to/project2.fs' | \
--     stack ghci fractalstream-backend-pure:lib \
--       --ghci-options /path/to/this/file/DeepCheck.hs
--
-- Prints, per file: whether it parsed, how many viewers/tools it has,
-- and whether each tool's event-handler dispatcher built successfully.
module Main where

import Actor.Ensemble
import Actor.UI
import Actor.Viewer
import Actor.Tool
import Actor.Event
import Actor.Configuration
import Language.Environment (SomeContext(..), SomeContext'(..))
import Data.DynamicValue
import Backend
import Control.Monad (forM_)
import Control.Exception (SomeException, try, evaluate)
import System.Environment (getArgs)

myUI :: UI
myUI = UI
  { newEnsemble = pure ()
  , runSetup = \_ _ _ k -> k
  , makeLayout = \_ _ _ -> pure (pure ())
  , makeViewer = \_ _ _ someCtx _ _ viewer -> do
      inspectViewer someCtx viewer
      pure (pure ())
  }

inspectViewer :: SomeContext EventArgument_ -> Viewer -> IO ()
inspectViewer ctx Viewer{..} = do
  tools <- getDynamic vTools
  putStrLn ("  viewer has " ++ show (length tools) ++ " tool(s)")
  forM_ tools $ \Tool{..} -> do
    n <- getDynamic (source $ tiName toolInfo)
    putStrLn ("  tool: " ++ n)
    -- Mirror ProjectViewer.hs's real context-building: a tool with its own
    -- `configuration:` panel gets that panel's variables combined with the
    -- ensemble-wide context, not just the ensemble-wide context alone.
    mtcfg <- getDynamic toolConfig
    toolCtx <- case mtcfg of
      Nothing -> pure (Right ctx)
      Just tcfg -> do
        toolContext0 <- layoutToArgs (coContents tcfg)
        pure $ case toolContext0 <> SomeContext' (Right ctx) of
          SomeContext' e -> e
    case toolCtx of
      Left err -> putStrLn ("    couldn't build tool context: " ++ err)
      Right fullCtx -> do
        r <- try (getDynamic (dyn toolScript) >>= vBuildToolHandler 0 fullCtx >>= evaluate)
        case r of
          Left (e :: SomeException) -> putStrLn ("    EXCEPTION: " ++ show e)
          Right (Left err) -> putStrLn ("    Left: " ++ err)
          Right (Right _)  -> putStrLn "    OK"

checkOne :: Backend -> FilePath -> IO ()
checkOne backend path = do
  putStrLn ("=== " ++ path ++ " ===")
  parseEnsembleFromFile path >>= \case
    Left err -> putStrLn ("parseEnsembleFromFile failed: " ++ err)
    Right ensemble -> do
      vs <- getDynamic (ensembleViewers ensemble)
      putStrLn ("ensemble has " ++ show (length vs) ++ " viewer(s)")
      r <- try (runEnsemble backend myUI ensemble)
      case r of
        Left (e :: SomeException) -> putStrLn ("runEnsemble EXCEPTION: " ++ show e)
        Right () -> putStrLn "runEnsemble returned normally"

main :: IO ()
main = withBackend $ \backend -> do
  paths <- getArgs
  mapM_ (checkOne backend) paths
