-- | The WebAssembly backend: viewers are compiled to wasm kernels at run
-- time (see "Backend.Wasm.Code") and loaded through the JavaScript FFI.
-- If the host gave the engine a pool of workers (@js/worker-pool.mjs@, at
-- @globalThis.fractalstreamWorkers@), a render's blocks are rendered there,
-- several at once (see 'workerPoolSize'); otherwise the kernel runs on the
-- engine's own thread, over its memory, writing a block's pixels straight
-- into its buffer. A viewer whose code the backend can't compile
-- yet is interpreted instead (by "Backend.Pure"), with a note on stderr.
-- Tools, and code that runs once per render, are interpreted.
module Backend.Wasm
  ( withWasmViewer
  , workerPoolSize
  ) where

import FractalStream.Prelude

import Backend.Wasm.Code
import Backend.Wasm.Encode (MemoryImport(..))
import Backend.Pure (interpretViewer)

import Actor.Autocolor (AutocolorLookup)
import Actor.Viewer
import Language.Code (Code, transformValues)
import Language.Environment
import Language.Type
import Language.Value.Transform (backendRewrites)
import Data.Color (colorToRGB)

import Control.Exception (evaluate)
import Data.Bits (testBit)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import GHC.Wasm.Prim (JSVal)
import System.IO (hPutStrLn, stderr)

-- | Compile and instantiate a kernel module from bytes in the engine's
-- memory, giving it that memory and the math functions it imports.
-- (Synchronous compilation is fine in Node and in a browser's worker,
-- where the engine runs; a browser's main thread only allows it for
-- small modules.)
foreign import javascript unsafe
  "new WebAssembly.Instance(new WebAssembly.Module(new Uint8Array(__exports.memory.buffer, $1, $2).slice()), { env: { memory: __exports.memory }, math: { sin: Math.sin, cos: Math.cos, tan: Math.tan, sinh: Math.sinh, cosh: Math.cosh, tanh: Math.tanh, asin: Math.asin, acos: Math.acos, atan: Math.atan, asinh: Math.asinh, acosh: Math.acosh, atanh: Math.atanh, exp: Math.exp, log: Math.log, pow: Math.pow, atan2: Math.atan2, hypot: Math.hypot, fmod: (a, b) => a % b } })"
  instantiate :: Ptr Word8 -> Int -> IO JSVal

foreign import javascript unsafe "$1.exports.kernel($2, $3, $4, $5, $6, $7, $8, $9, $10)"
  runKernel :: JSVal -> Ptr Word8 -> Int32 -> Int32 -> Int32 -> Ptr Word8
            -> Double -> Double -> Double -> Double -> IO Int32

-- | How many workers the host's pool has (0 without one).
foreign import javascript unsafe
  "globalThis.fractalstreamWorkers ? globalThis.fractalstreamWorkers.size : 0"
  workerPoolSize :: IO Int

foreign import javascript unsafe
  "new WebAssembly.Module(new Uint8Array(__exports.memory.buffer, $1, $2).slice())"
  compileKernel :: Ptr Word8 -> Int -> IO JSVal

foreign import javascript unsafe "globalThis.fractalstreamWorkers.addKernel($1)"
  addKernel :: JSVal -> IO Int

-- | Render a block on a worker, with a copy of the arguments; gives back
-- its result, to read with 'resultIssues' and 'copyPixels'. (The pixels
-- are copied into the engine's memory only after the result is forced,
-- by the thread that asked for it: a render cancelled while it waits must
-- not write into a buffer that's been freed.)
foreign import javascript safe
  "return await globalThis.fractalstreamWorkers.run($1, new Uint8Array(__exports.memory.buffer, $2, $3).slice(), $4, $5, $6, $7, $8, $9, $10)"
  runOnWorker :: Int -> Ptr Word8 -> Int -> Int32 -> Int32 -> Int32
              -> Double -> Double -> Double -> Double -> IO JSVal

foreign import javascript unsafe "$1.issues"
  resultIssues :: JSVal -> IO Int32

foreign import javascript unsafe
  "new Uint8Array(__exports.memory.buffer, $2, $1.pixels.length).set($1.pixels)"
  copyPixels :: JSVal -> Ptr Word8 -> IO ()

withWasmViewer :: forall env t. (MissingViewerArgs env, KnownEnvironment env)
               => AutocolorLookup
               -> Code (ViewerEnv env)
               -> (ViewerFunction env -> IO t) -> IO t
withWasmViewer _ code0 action = do
  let code = transformValues backendRewrites code0
  case compileViewer UnsharedMemory code of
    Left err -> do
      hPutStrLn stderr ("note: " ++ err ++ "; interpreting this viewer instead")
      interpretViewer code0 action
    Right bytes -> do
      workers <- workerPoolSize
      runBlock <- if workers > 0
        then do
          kernel <- withArrayLen bytes (\n p -> compileKernel p n) >>= addKernel
          pure $ \args argsLen ViewerArgs{..} -> do
            result <- runOnWorker kernel args argsLen vaWidth vaHeight vaSubsamples
                        (fst vaPoint) (snd vaPoint) (fst vaStep) (snd vaStep)
                      >>= evaluate
            copyPixels result vaBuffer
            resultIssues result
        else do
          inst <- withArrayLen bytes $ \n p -> instantiate p n
          pure $ \args _ ViewerArgs{..} ->
            runKernel inst vaBuffer vaWidth vaHeight vaSubsamples args
                      (fst vaPoint) (snd vaPoint) (fst vaStep) (snd vaStep)
      action $ ViewerFunction $ \vaArgs -> do
        -- The configuration's values, once for the whole render.
        let size = sum (fromContext (\_ ty _ -> fromMaybe 0 (argSize ty)) vaArgs)
        args <- mallocForeignPtrBytes (max 8 size)
        withForeignPtr args $ \p ->
          foldM_ (\off (SomeArg ty v) -> pokeArg p off ty v) 0
                 (fromContext (\_ ty v -> SomeArg ty v) vaArgs)
        pure $ \block -> withForeignPtr args $ \p -> do
          issues <- runBlock p size block
          pure ViewerIssues { viOutOfMemory = testBit issues 0
                            , viBadIndex = testBit issues 1 }

data SomeArg where
  SomeArg :: TypeProxy t -> HaskellType t -> SomeArg

-- | Write a value into the arguments buffer as 'argSize' lays it out,
-- giving the offset after it.
pokeArg :: Ptr Word8 -> Int -> TypeProxy t -> HaskellType t -> IO Int
pokeArg p off t v = case t of
  BooleanType -> pokeByteOff p off (if v then 1 else 0 :: Int32) >> pure (off + 8)
  IntegerType -> pokeByteOff p off (fromIntegral v :: Int32) >> pure (off + 8)
  RealType -> pokeByteOff p off v >> pure (off + 8)
  ComplexType -> do
    let x :+ y = v
    pokeByteOff p off x
    pokeByteOff p (off + 8) y
    pure (off + 16)
  ColorType -> do
    let (r, g, b) = colorToRGB v
    pokeByteOff p off r
    pokeByteOff p (off + 1) g
    pokeByteOff p (off + 2) b
    pure (off + 8)
  PairType a b -> let (x, y) = v in pokeArg p off a x >>= \off' -> pokeArg p off' b y
  -- (a viewer with any other type isn't compiled: see 'argSize')
  _ -> pure off
