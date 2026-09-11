module Backend
  ( withBackend
  ) where

import Actor.Viewer
import Backend.Pure

withBackend :: (ViewerCompiler -> IO a) -> IO a
withBackend action =
  action (ViewerCompiler interpretViewer)
