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
import Backend.Wasm.List (nodeSize, pairSecond)
import Backend.Wasm.Encode (MemoryImport(..))
import Backend.Pure (interpretViewer)

import Actor.Autocolor (AutocolorLookup(..), autocolorSnapshot)
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
import Data.IORef
import Foreign.Ptr (Ptr, ptrToWordPtr)
import Foreign.Storable (pokeByteOff, peekByteOff)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Wasm.Prim (JSVal)
import System.IO (hPutStrLn, stderr)
import qualified System.Environment as Env
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS

-- | Compile and instantiate a kernel module from bytes in the engine's
-- memory, giving it that memory and what it imports: the math functions
-- (the exports of @js/libm.wasm@, which the host puts at
-- @globalThis.fractalstreamMath@), and the autocolor lookup (an instance,
-- over the engine's memory, of @js/autocolor.wasm@, which the host puts,
-- compiled, at @globalThis.fractalstreamAutocolor@), as
-- @js/kernel-worker.mjs@ gives its kernels. (Synchronous compilation is
-- fine in Node and in a browser's worker, where the engine runs; a
-- browser's main thread only allows it for small modules.)
foreign import javascript unsafe
  "const memory = __exports.memory; return new WebAssembly.Instance(new WebAssembly.Module(new Uint8Array(memory.buffer, $1, $2).slice()), { env: { memory }, math: globalThis.fractalstreamMath, autocolor: new WebAssembly.Instance(globalThis.fractalstreamAutocolor, { env: { memory } }).exports });"
  instantiate :: Ptr Word8 -> Int -> IO JSVal

foreign import javascript unsafe "$1.exports.kernel($2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)"
  runKernel :: JSVal -> Ptr Word8 -> Int32 -> Int32 -> Int32 -> Ptr Word8 -> Ptr Word8 -> Int32
            -> Ptr Word8 -> Ptr Word8
            -> Double -> Double -> Double -> Double -> IO Int32

-- | How many workers the host's pool has (0 without one).
foreign import javascript unsafe
  "globalThis.fractalstreamWorkers ? globalThis.fractalstreamWorkers.size : 0"
  workerPoolSize :: IO Int

-- | Give the workers a kernel (its module's bytes: each compiles its own).
foreign import javascript unsafe
  "globalThis.fractalstreamWorkers.addKernel(new Uint8Array(__exports.memory.buffer, $1, $2).slice())"
  addKernel :: Ptr Word8 -> Int -> IO Int

-- | Render a block on a worker, with copies of the arguments and of the
-- autocolor snapshot; gives back its result, to read with
-- 'resultIssues', 'copyPixels', and 'copyMisses'. (They're copied into
-- the engine's memory only after the result is forced, by the thread
-- that asked for it: a render cancelled while it waits must not write
-- into a buffer that's been freed.)
foreign import javascript safe
  "const m = __exports.memory.buffer; return await globalThis.fractalstreamWorkers.run($1, new Uint8Array(m, $2, $3).slice(), new Uint8Array(m, $4, $5).slice(), $6, $7, $8, $9, $10, $11, $12)"
  runOnWorker :: Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Int32 -> Int32 -> Int32
              -> Double -> Double -> Double -> Double -> IO JSVal

foreign import javascript unsafe "$1.issues"
  resultIssues :: JSVal -> IO Int32

foreign import javascript unsafe
  "new Uint8Array(__exports.memory.buffer, $2, $1.pixels.length).set($1.pixels)"
  copyPixels :: JSVal -> Ptr Word8 -> IO ()

-- | Copy a worker's misses (as many as fit) into a misses list in the
-- engine's memory (see 'readMisses').
foreign import javascript unsafe
  "const n = Math.min($1.misses.length / 24, $3); new Uint8Array(__exports.memory.buffer, $2 + 8, n * 24).set($1.misses.subarray(0, n * 24)); new Int32Array(__exports.memory.buffer, $2, 1)[0] = n;"
  copyMisses :: JSVal -> Ptr Word8 -> Int -> IO ()

withWasmViewer :: forall env t. (MissingViewerArgs env, KnownEnvironment env)
               => AutocolorLookup
               -> Code (ViewerEnv env)
               -> (ViewerFunction env -> IO t) -> IO t
withWasmViewer lookupAutocolor code0 action = do
  let code = transformValues backendRewrites code0
  case compileViewer UnsharedMemory code of
    Left err -> do
      hPutStrLn stderr ("note: " ++ err ++ "; interpreting this viewer instead")
      interpretViewer code0 action
    Right bytes -> do
      -- (for debugging: FS_WASM_DUMP=folder writes each kernel there)
      Env.lookupEnv "FS_WASM_DUMP" >>= mapM_ (\dir -> do
        n <- atomicModifyIORef' dumpCounter (\k -> (k + 1, k))
        BS.writeFile (dir ++ "/kernel-" ++ show n ++ ".wasm") (BS.pack bytes))
      workers <- workerPoolSize
      -- How to run a block once, with the current autocolor snapshot,
      -- leaving the points it would add in the misses list; and where a
      -- block's arguments will be, so lists in them can point into them:
      -- their own address on this thread, or 'workerArgsBase'.
      (argsBase, runOnce) <- if workers > 0
        then do
          kernel <- withArrayLen bytes (\n p -> addKernel p n)
          pure $ (,) (const workerArgsBase) $ \args argsLen misses ViewerArgs{..} -> do
            (snapshot, snapshotLen) <- autocolorSnapshot lookupAutocolor
            result <- runOnWorker kernel args argsLen snapshot snapshotLen
                        vaWidth vaHeight vaSubsamples
                        (fst vaPoint) (snd vaPoint) (fst vaStep) (snd vaStep)
                      >>= evaluate
            copyPixels result vaBuffer
            copyMisses result misses missCapacity
            resultIssues result
        else do
          inst <- withArrayLen bytes $ \n p -> instantiate p n
          -- (one block at a time on this thread, so one arena will do)
          arena <- mallocForeignPtrBytes arenaSize
          pure $ (,) (fromIntegral . ptrToWordPtr) $ \args _ misses ViewerArgs{..} ->
            withForeignPtr arena $ \a -> do
              -- (the snapshot itself: nothing can replace it while the
              -- kernel runs, on this one thread)
              (snapshot, _) <- autocolorSnapshot lookupAutocolor
              runKernel inst vaBuffer vaWidth vaHeight vaSubsamples args a (fromIntegral arenaSize)
                        snapshot misses
                        (fst vaPoint) (snd vaPoint) (fst vaStep) (snd vaStep)
      action $ ViewerFunction $ \vaArgs -> do
        -- The configuration's values, once for the whole render: a slot
        -- for each, then the nodes of any lists.
        let values = fromContext (\_ ty v -> SomeArg ty v) vaArgs
            slots = sum [ fromMaybe 0 (argSize ty) | SomeArg ty _ <- values ]
            size = slots + sum [ valueListBytes ty v | SomeArg ty v <- values ]
        args <- mallocForeignPtrBytes (max 8 size)
        withForeignPtr args $ \p -> do
          heap <- newIORef slots
          let base = argsBase p
          foldM_ (\off (SomeArg ty v) -> pokeArg p base heap off ty v) 0 values
        -- A block that met points to add (autocolor) is drawn again, once
        -- they're added, until it meets none: so it comes out as with the
        -- LLVM backend, which adds them as it goes.
        pure $ \block -> withForeignPtr args $ \p ->
          allocaBytes (8 + missSize * missCapacity) $ \misses -> do
            let go :: Int -> IO Int32
                go tries = do
                  pokeByteOff misses 0 (0 :: Int32)
                  pokeByteOff misses 4 (fromIntegral missCapacity :: Int32)
                  issues <- runOnce p size misses block
                  missed <- readMisses misses
                  if null missed || tries >= maxRedraws
                    then pure issues
                    else do
                      forM_ missed $ \(mode, n, x, y) -> autocolorFunction lookupAutocolor mode n x y
                      go (tries + 1)
            issues <- go 0
            pure ViewerIssues { viOutOfMemory = testBit issues 0
                              , viBadIndex = testBit issues 1 }

-- | How many points a block can note to add before it's drawn again, and
-- how many times it's drawn again at most. (Each time adds every point it
-- noted, or finds them already added; a group stops adding at 256.)
missCapacity, maxRedraws :: Int
missCapacity = 256
maxRedraws = 8

-- | A noted point: its group, mode, and coordinates (see
-- @libm/fsautocolor.c@).
missSize :: Int
missSize = 24

readMisses :: Ptr Word8 -> IO [(AutocolorMode, Int, Double, Double)]
readMisses p = do
  n <- peekByteOff p 0 :: IO Int32
  forM [0 .. fromIntegral n - 1] $ \k -> do
    let e = 8 + missSize * k
    group <- peekByteOff p e :: IO Int32
    code <- peekByteOff p (e + 4) :: IO Int32
    x <- peekByteOff p (e + 8)
    y <- peekByteOff p (e + 16)
    let mode = fromMaybe AddPoint (lookup (fromIntegral code)
                 [ (autocolorModeCode m, m) | m <- [minBound .. maxBound] ])
    pure (mode, fromIntegral group, x, y)

data SomeArg where
  SomeArg :: TypeProxy t -> HaskellType t -> SomeArg

dumpCounter :: IORef Int
dumpCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE dumpCounter #-}

-- | Where a worker puts a block's arguments in its memory (see
-- @js/kernel-worker.mjs@).
workerArgsBase :: Int
workerArgsBase = 16

-- | Each block's scratch memory for lists, as the LLVM backend's.
arenaSize :: Int
arenaSize = 1024 * 1024

-- | How many bytes the nodes of a value's lists take (see
-- "Backend.Wasm.List"), if it has any.
valueListBytes :: TypeProxy t -> HaskellType t -> Int
valueListBytes t v = case t of
  ListType i -> fromMaybe 0 (nodeSize i) * length v + sum (map (valueListBytes i) v)
  PairType a b -> let (x, y) = v in valueListBytes a x + valueListBytes b y
  _ -> 0

-- | Write a value into the arguments buffer at @p@ as 'argSize' lays it
-- out, giving the offset after it. A list's nodes go at the heap's next
-- free offset, and the slot holds its address, where the buffer will be
-- once it's at @base@.
pokeArg :: Ptr Word8 -> Int -> IORef Int -> Int -> TypeProxy t -> HaskellType t -> IO Int
pokeArg p base heap off t v = case t of
  ListType i -> do
    addr <- pokeList p base heap i v
    pokeByteOff p off (fromIntegral addr :: Int32)
    pure (off + 8)
  PairType a b -> let (x, y) = v in pokeArg p base heap off a x >>= \off' -> pokeArg p base heap off' b y
  _ -> pokeScalar p off t v >> pure (off + fromMaybe 0 (argSize t))

-- | A list's nodes, at the heap's next free offset; gives its address
-- (where the buffer will be once it's at @base@), or 0 if it's empty.
pokeList :: Ptr Word8 -> Int -> IORef Int -> TypeProxy t -> [HaskellType t] -> IO Int
pokeList _ _ _ _ [] = pure 0
pokeList p base heap t xs = do
  let stride = fromMaybe 0 (nodeSize t)
      n = length xs
  start <- atomicModifyIORef' heap (\h -> (h + stride * n, h))
  forM_ (zip [0 ..] xs) $ \(k, x) -> do
    let node = start + k * stride
        next = if k == n - 1 then 0 else base + node + stride
    pokeByteOff p node (fromIntegral next :: Int32)
    pokeElem p base heap (node + 8) t x
  pure (base + start)

-- | A list element, as "Backend.Wasm.List" lays it out.
pokeElem :: Ptr Word8 -> Int -> IORef Int -> Int -> TypeProxy t -> HaskellType t -> IO ()
pokeElem p base heap off t v = case t of
  ListType i -> pokeList p base heap i v >>= \a -> pokeByteOff p off (fromIntegral a :: Int32)
  PairType a b -> do
    let (x, y) = v
    pokeElem p base heap off a x
    pokeElem p base heap (off + fromMaybe 0 (pairSecond a)) b y
  _ -> pokeScalar p off t v

-- | A truth value, integer, real or complex number, or color.
pokeScalar :: Ptr Word8 -> Int -> TypeProxy t -> HaskellType t -> IO ()
pokeScalar p off t v = case t of
  BooleanType -> pokeByteOff p off (if v then 1 else 0 :: Int32)
  IntegerType -> pokeByteOff p off (fromIntegral v :: Int32)
  RealType -> pokeByteOff p off v
  ComplexType -> do
    let x :+ y = v
    pokeByteOff p off x
    pokeByteOff p (off + 8) y
  ColorType -> do
    let (r, g, b) = colorToRGB v
    pokeByteOff p off r
    pokeByteOff p (off + 1) g
    pokeByteOff p (off + 2) b
  -- (a viewer with any other type isn't compiled: see 'argSize')
  _ -> pure ()
