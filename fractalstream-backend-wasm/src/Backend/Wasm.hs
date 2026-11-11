-- | The WebAssembly backend: viewers are compiled to wasm kernels at run
-- time (see "Backend.Wasm.Code"), loaded through the JavaScript FFI, and
-- run over the engine's own memory, so a kernel writes its pixels straight
-- into the block's buffer. A viewer whose code the backend can't compile
-- yet is interpreted instead (by "Backend.Pure"), with a note on stderr.
-- Tools, and code that runs once per render, are interpreted.
module Backend.Wasm
  ( withWasmViewer
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
      inst <- withArrayLen bytes $ \n p -> instantiate p n
      action $ ViewerFunction $ \vaArgs -> do
        -- The configuration's values, once for the whole render.
        let size = sum (fromContext (\_ ty _ -> fromMaybe 0 (argSize ty)) vaArgs)
        args <- mallocForeignPtrBytes (max 8 size)
        withForeignPtr args $ \p ->
          foldM_ (\off (SomeArg ty v) -> pokeArg p off ty v) 0
                 (fromContext (\_ ty v -> SomeArg ty v) vaArgs)
        pure $ \ViewerArgs{..} -> withForeignPtr args $ \p -> do
          issues <- runKernel inst vaBuffer vaWidth vaHeight vaSubsamples p
                              (fst vaPoint) (snd vaPoint) (fst vaStep) (snd vaStep)
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
