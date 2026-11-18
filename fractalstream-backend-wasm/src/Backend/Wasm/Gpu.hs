-- | The web's GPU, for the GPU backends ("Backend.WGSL"): WebGPU, through
-- the host's @js/gpu.mjs@, which the engine's worker puts at
-- @globalThis.fractalstreamGpu@ (in Node, there's none).
module Backend.Wasm.Gpu
  ( webGpu
  ) where

import Backend.WGSL.Device

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_, when)
import Data.List (intercalate)
import Foreign.ForeignPtr (withForeignPtr)
import Data.Word (Word8, Word32)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr, nullPtr)
import GHC.Wasm.Prim (JSString(..), JSVal, toJSString, fromJSString)

-- | Why there's no GPU ("" if there is one).
foreign import javascript unsafe
  "const g = globalThis.fractalstreamGpu; return g ? (g.error ?? '') : 'No WebGPU here'"
  gpuError :: IO JSString

foreign import javascript safe "return await globalThis.fractalstreamGpu.compile($1)"
  gpuCompile :: JSString -> IO JSVal

foreign import javascript unsafe
  "globalThis.fractalstreamGpu.bind($1, new Uint32Array(__exports.memory.buffer, ($2 >>> 0), $3).slice(), $4)"
  gpuBind :: JSVal -> Ptr Word32 -> Int -> JSString -> IO JSVal

foreign import javascript unsafe "globalThis.fractalstreamGpu.hasTables($1)"
  gpuHasTables :: JSString -> IO Bool

-- | Room for a set of tables: why not ("" if there is).
foreign import javascript unsafe "globalThis.fractalstreamGpu.newTables($1, $2) ?? ''"
  gpuNewTables :: JSString -> Int -> IO JSString

foreign import javascript unsafe
  "globalThis.fractalstreamGpu.writeTable($1, $2, new Uint8Array(__exports.memory.buffer, ($3 >>> 0), $4))"
  gpuWriteTable :: JSString -> Int -> Ptr Word8 -> Int -> IO ()

-- | A block's pixels, as RGB rows (copied into the engine's memory with
-- 'copyRgb' only once it's done: memory can grow while it waits); with a
-- palette (its words; a null pointer for none), the points to add too
-- ('copyMisses', this many bytes of them).
foreign import javascript safe
  "return await globalThis.fractalstreamGpu.run($1, new Uint8Array(__exports.memory.buffer, ($2 >>> 0), 80).slice(), $3, $4, $5 ? new Uint32Array(__exports.memory.buffer, ($5 >>> 0), $6).slice() : null, $7)"
  gpuRun :: JSVal -> Ptr Word8 -> Int -> Int -> Ptr Word32 -> Int -> Int -> IO JSVal

foreign import javascript unsafe "if ($1.misses) new Uint8Array(__exports.memory.buffer, ($2 >>> 0), $1.misses.length).set($1.misses)"
  copyMisses :: JSVal -> Ptr Word8 -> IO ()

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer, ($2 >>> 0), $1.pixels.length).set($1.pixels)"
  copyRgb :: JSVal -> Ptr Word8 -> IO ()

foreign import javascript unsafe "$1.issues"
  resultIssues :: JSVal -> IO Int

-- | Why a compile or a run failed ("" if it didn't).
foreign import javascript unsafe "$1.error ?? ''"
  failed :: JSVal -> IO JSString

-- | The GPU, or why there's none.
webGpu :: IO (Either String GpuDevice)
webGpu = fromJSString <$> gpuError >>= \case
  "" -> pure (Right GpuDevice { gdCompile = compile, gdF64 = False, gdName = "WebGPU" })
  why -> pure (Left why)
  where
    compile source = attempt (gpuCompile (toJSString source)) >>= \case
      Left err -> pure (Left err)
      Right pipeline -> pure (Right GpuKernel { gkBind = bind pipeline })
    bind pipeline ws tables = do
      -- (a set of tables, by their versions and places, is uploaded once)
      let key = toJSString (intercalate "," [ show (gtVersion t) ++ "@" ++ show (gtOffset t) | t <- tables ])
          size = maximum (0 : [ gtOffset t + gtSize t | t <- tables ])
      has <- gpuHasTables key
      problem <- if has then pure "" else do
        why <- fromJSString <$> gpuNewTables key size
        when (null why) $ forM_ tables $ \t ->
          withForeignPtr (gtBytes t) $ \p -> gpuWriteTable key (gtOffset t) p (gtSize t)
        pure why
      args <- withArrayLen ws (\n p -> gpuBind pipeline p n key)
      pure GpuArgs { gaRun = \params w h out colors ->
        if not (null problem) then pure (Left problem) else do
          let (palette, n) = maybe (nullPtr, 0) (\c -> (gacPalette c, gacWords c)) colors
          r <- attempt (gpuRun args params w h palette n missesBytes)
          forM r $ \result -> do
            copyRgb result out
            forM_ colors (copyMisses result . gacMisses)
            fromIntegral <$> resultIssues result }
    -- (a failure the host reports, or, failing that, one it throws)
    attempt :: IO JSVal -> IO (Either String JSVal)
    attempt act = try (act >>= evaluate) >>= \case
      Left (e :: SomeException) -> pure (Left (takeWhile (/= '\n') (show e)))
      Right v -> fromJSString <$> failed v >>= \case
        "" -> pure (Right v)
        why -> pure (Left why)
