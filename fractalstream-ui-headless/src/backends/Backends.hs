{-# LANGUAGE CPP #-}
-- | fsrun's backends (and the end-to-end tests'), as its cabal flags
-- choose them: one CPU backend (WebAssembly's on wasm32; otherwise LLVM's,
-- or the interpreter with @-use-llvm@), and, with the @gpu@ flag (the
-- default), the GPU backends on the platform's device (on wasm32, the
-- host's WebGPU, if it has one; elsewhere wgpu-native, where this build
-- has it: macOS and Linux, for now).
module Backends
  ( withBackend
  ) where

import Actor.Viewer (Backend, CpuBackend, ViewerBackends, withBackends)

#if defined(wasm32_HOST_ARCH)
import Backend.Wasm (wasmBackend)
#elif defined(USE_LLVM)
import Backend.LLVM (llvmBackend)
#else
import Backend.Pure (pureBackend)
#endif

#if defined(USE_GPU)
import Backend.WGSL (gpuBackends)
#if defined(wasm32_HOST_ARCH)
import Backend.Wasm.Gpu (webGpu)
#else
import Backend.WGPU (nativeGpu)
#endif
#endif

-- | Run with this build's backends.
withBackend :: (Backend -> IO a) -> IO a
withBackend = withBackends cpuBackend viewerBackends

cpuBackend :: CpuBackend
#if defined(wasm32_HOST_ARCH)
cpuBackend = wasmBackend
#elif defined(USE_LLVM)
cpuBackend = llvmBackend
#else
cpuBackend = pureBackend
#endif

viewerBackends :: [ViewerBackends]
#if defined(USE_GPU) && defined(wasm32_HOST_ARCH)
viewerBackends = [gpuBackends webGpu]
#elif defined(USE_GPU)
viewerBackends = [ gpuBackends device | Just device <- [nativeGpu] ]
#else
viewerBackends = []
#endif
