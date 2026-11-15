-- | The web's GPU, for the GPU backends ("Backend.WGSL"): WebGPU, through
-- the host's @js/gpu.mjs@, which the engine's worker puts at
-- @globalThis.fractalstreamGpu@ (in Node, there's none).
module Backend.Wasm.Gpu
  ( webGpu
  ) where

import Backend.WGSL.Device

import Control.Exception (SomeException, evaluate, try)
import Data.Word (Word8, Word32)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr)
import GHC.Wasm.Prim (JSString(..), JSVal, toJSString, fromJSString)

-- | Why there's no GPU ("" if there is one).
foreign import javascript unsafe
  "const g = globalThis.fractalstreamGpu; return g ? (g.error ?? '') : 'No WebGPU here'"
  gpuError :: IO JSString

foreign import javascript safe "return await globalThis.fractalstreamGpu.compile($1)"
  gpuCompile :: JSString -> IO JSVal

foreign import javascript unsafe
  "globalThis.fractalstreamGpu.bind($1, new Uint32Array(__exports.memory.buffer, $2, $3).slice())"
  gpuBind :: JSVal -> Ptr Word32 -> Int -> IO JSVal

-- | A block's pixels, as RGB rows (copied into the engine's memory with
-- 'copyRgb' only once it's done: memory can grow while it waits).
foreign import javascript safe
  "return await globalThis.fractalstreamGpu.run($1, new Uint8Array(__exports.memory.buffer, $2, 80).slice(), $3, $4)"
  gpuRun :: JSVal -> Ptr Word8 -> Int -> Int -> IO JSVal

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer, $2, $1.length).set($1)"
  copyRgb :: JSVal -> Ptr Word8 -> IO ()

-- | Why a compile or a run failed ("" if it didn't).
foreign import javascript unsafe "$1.error ?? ''"
  failed :: JSVal -> IO JSString

-- | The GPU, or why there's none.
webGpu :: IO (Either String GpuDevice)
webGpu = fromJSString <$> gpuError >>= \case
  "" -> pure (Right GpuDevice { gdCompile = compile })
  why -> pure (Left why)
  where
    compile source = attempt (gpuCompile (toJSString source)) >>= \case
      Left err -> pure (Left err)
      Right pipeline -> pure (Right GpuKernel { gkBind = bind pipeline })
    bind pipeline ws = do
      args <- withArrayLen ws (\n p -> gpuBind pipeline p n)
      pure GpuArgs { gaRun = \params w h out -> do
        r <- attempt (gpuRun args params w h)
        traverse (\rgb -> copyRgb rgb out) r }
    -- (a failure the host reports, or, failing that, one it throws)
    attempt :: IO JSVal -> IO (Either String JSVal)
    attempt act = try (act >>= evaluate) >>= \case
      Left (e :: SomeException) -> pure (Left (takeWhile (/= '\n') (show e)))
      Right v -> fromJSString <$> failed v >>= \case
        "" -> pure (Right v)
        why -> pure (Left why)
