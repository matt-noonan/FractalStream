module Backend
  ( withBackend
  , module Actor.Viewer   -- re-export Backend, ToolRunnerFactory, etc.
  ) where

import Actor.Viewer
import Actor.Event (interpretedToolExec)
import Language.Draw (ignoreDrawing)
import Backend.Pure

withBackend :: (Backend -> IO a) -> IO a
withBackend action =
  action Backend
    { bViewerCompiler    = ViewerCompiler (const interpretViewer)
    , bToolRunnerFactory = defaultToolRunnerFactory
    , bToolRunner        = defaultToolRunner
    , bRunOnce           = \_ -> interpretedToolExec ignoreDrawing
    }
