module Backend
  ( withBackend
  , module Actor.Viewer   -- re-export Backend, ToolRunnerFactory, etc.
  ) where

import Actor.Viewer
import Backend.LLVM

withBackend :: (Backend -> IO a) -> IO a
withBackend action = withJIT $ \jit ->
  action Backend
    { bViewerCompiler    = ViewerCompiler (withJittedViewer jit)
    , bToolRunnerFactory = defaultToolRunnerFactory
    }
