module Backend
  ( withBackend
  ) where

import Actor.Viewer
import Backend.LLVM

withBackend :: (ViewerCompiler -> IO a) -> IO a
withBackend action = withJIT $ \jit ->
  action (ViewerCompiler (withJittedViewer jit))
