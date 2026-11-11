module Backend
  ( withBackend
  , module Actor.Viewer   -- re-export Backend, ToolRunnerFactory, etc.
  ) where

import Actor.Viewer
import Language.Draw (ignoreDrawing)
import Backend.LLVM

withBackend :: (Backend -> IO a) -> IO a
withBackend action = withJIT $ \jit ->
  action Backend
    { bViewerCompiler    = ViewerCompiler (withJittedViewer jit)
    , bToolRunnerFactory = defaultToolRunnerFactory
    , bToolRunner        = llvmToolRunner jit
    , bRunOnce           = \lookupAutocolor -> compiledToolExec jit lookupAutocolor ignoreDrawing
    }
