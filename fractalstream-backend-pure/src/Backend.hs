module Backend
  ( withBackend
  , module Actor.Viewer   -- re-export Backend, ToolRunnerFactory, etc.
  ) where

import Actor.Viewer
import FractalStream.Backends (BackendId(..))
import Actor.Event (interpretedToolExec)
import Language.Draw (ignoreDrawing)
import Backend.Pure

withBackend :: (Backend -> IO a) -> IO a
withBackend action =
  action Backend
    { bViewerCompilers   = [(Cpu, ViewerCompiler (\_ c -> Right . cpuViewer <$> interpretedViewer c))]
    , bToolRunnerFactory = defaultToolRunnerFactory
    , bToolRunner        = defaultToolRunner
    , bRunOnce           = \_ -> interpretedToolExec ignoreDrawing
    }
