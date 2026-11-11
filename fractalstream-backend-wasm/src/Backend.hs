module Backend
  ( withBackend
  , module Actor.Viewer   -- re-export Backend, ToolRunnerFactory, etc.
  ) where

import Actor.Viewer
import Actor.Event (interpretedToolExec)
import Language.Draw (ignoreDrawing)
import Backend.Wasm

-- | Viewers are compiled to WebAssembly; tools, and the code that runs
-- once per render, are interpreted (as with the pure backend).
withBackend :: (Backend -> IO a) -> IO a
withBackend action =
  action Backend
    { bViewerCompiler    = ViewerCompiler withWasmViewer
    , bToolRunnerFactory = defaultToolRunnerFactory
    , bToolRunner        = defaultToolRunner
    , bRunOnce           = \_ -> interpretedToolExec ignoreDrawing
    }
