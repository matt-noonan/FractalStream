-- | Write every viewer's GPU shaders (both precisions) for the projects
-- listed in a file (one path a line) into a folder, for checking them with
-- other compilers (dev-tools/gpu/naga-check.ts). Run from
-- fractalstream-backend-wgsl, with its test sources (for the pure backend):
--
--   stack exec --package hspec -- runghc -itest ../dev-tools/gpu/dump-shaders.hs DIR LIST
{-# LANGUAGE LambdaCase #-}
-- Dump every viewer's GPU shaders (both precisions) for the given project files.
import Backend.Pure (pureBackend)
import Backend.WGSL
import Actor.Ensemble (loadProjectBytes, LoadedProject(..), runEnsemble)
import Actor.UI (UI(..))
import Actor.Viewer
import FractalStream.Backends
import Control.Monad (forM_)
import Data.IORef
import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.FilePath (takeBaseName, (</>))

main :: IO ()
main = do
  [out, listFile] <- getArgs
  files <- lines <$> readFile listFile
  forM_ files $ \f -> do
    bytes <- BS.readFile f
    loadProjectBytes f bytes >>= \case
      Right (Built ens) -> do
        kept <- newIORef []
        viewers <- newIORef []
        let device = GpuDevice { gdF64 = True, gdName = "", gdCompile = \src -> do
              modifyIORef kept (src :)
              pure (Right (GpuKernel (\_ _ -> pure (GpuArgs (\_ _ _ _ _ -> pure (Left "no")))))) }
            ui = UI { newEnsemble = pure (), makeLayout = \_ _ _ -> pure (pure ())
                    , makeViewer = \_ _ _ _ _ v -> modifyIORef viewers (v :) >> pure (pure ()) }
        withBackends pureBackend [gpuBackends allPrecisions (pure (Right device))] $ \backend -> do
          runEnsemble backend ui ens
          vs <- readIORef viewers
          forM_ vs $ \v -> viewerRenderer v >>= \case
            Right (Renderer tries) -> forM_ tries $ \(b, go) -> if b == Cpu then pure () else () <$ go
            Left _ -> pure ()
          srcs <- readIORef kept
          forM_ (zip [0 :: Int ..] (reverse srcs)) $ \(k, s) ->
            writeFile (out </> (takeBaseName f ++ "-" ++ show k ++ ".wgsl")) s
          putStrLn (f ++ ": " ++ show (length srcs) ++ " shaders")
      _ -> putStrLn (f ++ ": didn't build")
