module Backend
  ( withBackend
  , module Actor.Viewer   -- re-export Backend, ToolRunnerFactory, etc.
  ) where

import Actor.Viewer
import Control.Monad (when)
import Actor.Event (interpretedToolExec)
import Language.Draw (ignoreDrawing)
import Backend.Wasm
import Task.Concurrent (defaultRenderConcurrency)

-- | Viewers and tools are compiled to WebAssembly; the code that runs
-- once per render is interpreted (as with the pure backend). A render
-- works on as many blocks at once as the host has workers (unless the
-- frontend says otherwise, as fsrun's @-j@ does).
withBackend :: (Backend -> IO a) -> IO a
withBackend action = do
  workers <- workerPoolSize
  when (workers > 0) (defaultRenderConcurrency workers)
  action Backend
    { bViewerCompiler    = ViewerCompiler wasmViewer
    , bToolRunnerFactory = defaultToolRunnerFactory
    , bToolRunner        = wasmToolRunner
    , bRunOnce           = \_ -> interpretedToolExec ignoreDrawing
    }
