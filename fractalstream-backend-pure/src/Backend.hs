module Backend
  ( withBackend
  ) where

import Actor.Viewer.Complex
import Backend.Pure

withBackend :: (ComplexViewerCompiler -> IO a) -> IO a
withBackend action =
  action (ComplexViewerCompiler interpretComplexViewer)
b
