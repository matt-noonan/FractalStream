module Backend
  ( withBackend
  , module Actor.Viewer   -- re-export Backend, ToolRunnerFactory, etc.
  ) where

import Actor.Viewer
import Backend.Pure

withBackend :: (Backend -> IO a) -> IO a
withBackend action =
  action Backend
    { bViewerCompiler    = ViewerCompiler interpretViewer
    , bToolRunnerFactory = defaultToolRunnerFactory
    , bToolRunner        = defaultToolRunner
    }
