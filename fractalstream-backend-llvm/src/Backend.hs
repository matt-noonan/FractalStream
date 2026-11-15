module Backend
  ( withBackend
  , module Actor.Viewer   -- re-export Backend, ToolRunnerFactory, etc.
  ) where

import Actor.Viewer
import FractalStream.Backends (BackendId(..))
import Language.Draw (ignoreDrawing)
import Backend.LLVM
import Backend.WGSL (gpuViewerCompilers)
import Backend.WGPU (nativeGpu)
import System.IO.Unsafe (unsafeInterleaveIO)

-- | The CPU backend (LLVM), and, where there's a GPU device for this
-- platform (macOS), the GPU backends. The GPU starts only when a GPU
-- backend first compiles a viewer: a session drawn on the CPU alone
-- (fsrun's default) never touches it.
withBackend :: (Backend -> IO a) -> IO a
withBackend action = withJIT $ \jit -> do
  gpu <- traverse unsafeInterleaveIO nativeGpu
  action Backend
    { bViewerCompilers   = (Cpu, ViewerCompiler (\a c -> Right . cpuViewer <$> jittedViewer jit a c))
                         : maybe [] gpuViewerCompilers gpu
    , bToolRunnerFactory = defaultToolRunnerFactory
    , bToolRunner        = llvmToolRunner jit
    , bRunOnce           = \lookupAutocolor -> compiledToolExec jit lookupAutocolor ignoreDrawing
    }
