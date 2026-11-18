{-# LANGUAGE CPP #-}
-- | The web app's backends, as its cabal flags choose them: WebAssembly's,
-- on the CPU, and, with the @gpu@ flag (the default), the GPU backends on
-- the browser's WebGPU, if it has one.
module Backends
  ( withBackend
  ) where

import Actor.Viewer (Backend, ViewerBackends, withBackends)
import Backend.Wasm (wasmBackend)

#if defined(USE_GPU)
import Backend.WGSL (gpuBackends, Precision(..))
import Backend.Wasm.Gpu (webGpu)
#endif

-- | Run with this build's backends.
withBackend :: (Backend -> IO a) -> IO a
withBackend = withBackends wasmBackend viewerBackends

viewerBackends :: [ViewerBackends]
#if defined(USE_GPU)
viewerBackends = [gpuBackends [DoubleFloat, Single] webGpu]   -- (WebGPU has no doubles)
#else
viewerBackends = []
#endif
