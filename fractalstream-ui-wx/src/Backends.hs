{-# LANGUAGE CPP #-}
-- | The app's backends, as its cabal flags choose them: one CPU backend
-- (LLVM's, or the interpreter with @-use-llvm@), and, with the @gpu@ flag
-- (the default), the GPU backends on wgpu-native, where this build has it
-- (macOS, for now).
module Backends
  ( withBackend
  ) where

import Actor.Viewer (Backend, CpuBackend, ViewerBackends, withBackends)

#if defined(USE_LLVM)
import Backend.LLVM (llvmBackend)
#else
import Backend.Pure (pureBackend)
#endif

#if defined(USE_GPU)
import Backend.WGSL (gpuBackends)
import Backend.WGPU (nativeGpu)
#endif

-- | Run with this build's backends.
withBackend :: (Backend -> IO a) -> IO a
withBackend = withBackends cpuBackend viewerBackends

cpuBackend :: CpuBackend
#if defined(USE_LLVM)
cpuBackend = llvmBackend
#else
cpuBackend = pureBackend
#endif

viewerBackends :: [ViewerBackends]
#if defined(USE_GPU)
viewerBackends = [ gpuBackends device | Just device <- [nativeGpu] ]
#else
viewerBackends = []
#endif
