module Backend
  ( withBackend
  , module Actor.Viewer   -- re-export Backend, ToolRunnerFactory, etc.
  ) where

import Actor.Viewer
import FractalStream.Backends (BackendId(..))
import Language.Draw (ignoreDrawing)
import Backend.LLVM

withBackend :: (Backend -> IO a) -> IO a
withBackend action = withJIT $ \jit ->
  action Backend
    { bViewerCompilers   = [(Cpu, ViewerCompiler (\a c -> Right . cpuViewer <$> jittedViewer jit a c))]
    , bToolRunnerFactory = defaultToolRunnerFactory
    , bToolRunner        = llvmToolRunner jit
    , bRunOnce           = \lookupAutocolor -> compiledToolExec jit lookupAutocolor ignoreDrawing
    }
