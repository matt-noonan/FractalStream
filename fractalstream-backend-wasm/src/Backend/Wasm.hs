-- | The WebAssembly backend: viewers are compiled to wasm kernels at run
-- time (see "Backend.Wasm.Code") and loaded through the JavaScript FFI.
-- If the host gave the engine a pool of workers (@js/worker-pool.mjs@, at
-- @globalThis.fractalstreamWorkers@), a render's blocks are rendered there,
-- several at once (see 'workerPoolSize'); otherwise the kernel runs on the
-- engine's own thread, over its memory, writing a block's pixels straight
-- into its buffer. A viewer whose code the backend can't compile
-- yet is interpreted instead (by "Backend.Pure"), with a note on stderr.
--
-- Tools' event handlers are compiled too ('wasmToolRunner'), and run on
-- the workers, one at a time each; a tool the backend can't compile
-- fails to build. (Without workers, tools are interpreted.) Code that
-- runs once per render is interpreted.
module Backend.Wasm
  ( withWasmViewer
  , workerPoolSize
  , wasmToolRunner
  ) where

import FractalStream.Prelude

import Backend.Wasm.Code
import Backend.Wasm.List (nodeSize, pairSecond)
import Backend.Wasm.Text (literalNode, numberNode, concatNode, utf8Bytes, literalNodeBytes)
import Backend.Wasm.Encode (MemoryImport(..))
import Backend.Pure (interpretViewer)

import Actor.Autocolor (AutocolorLookup(..), autocolorSnapshot)
import Actor.Event (ToolExec, HandlerRun(..), EventArgument(..), HandlerCancelled(..), runCancelled,
                    buildHandlerWith, snapshotEventArgs)
import Actor.Viewer
import Language.Draw (DrawSink(..))
import Language.Code (Code, CodeF(..), transformValues, usedVarsInCode)
import Data.Indexed.Functor (indexedFoldM)
import Language.Table (Table(..), elementSize, newTable, freezeTable)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Backend.Wasm.Table (descriptorBytes)
import qualified Data.Set as Set
import Language.Environment
import Language.Type
import Language.Value.Transform (backendRewrites)
import Data.Color (colorToRGB, rgbToColor)

import Control.Exception (Exception, SomeException, evaluate, throwIO, handle, try, bracket, finally)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.Bits (testBit)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr, touchForeignPtr)
import Language.Value.Evaluator (HaskellValue)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Array (withArrayLen)
import Data.IORef
import Foreign.Ptr (Ptr, ptrToWordPtr, plusPtr)
import Foreign.Storable (pokeByteOff, peekByteOff)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Wasm.Prim (JSVal)
import System.IO (hPutStrLn, stderr)
import qualified System.Environment as Env
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (utf8)
import Foreign.Ptr (castPtr)

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

-- | Run a tool's handler on a worker, with a copy of its arguments and
-- an arena of the given size (see @js/kernel-worker.mjs@), tagged (to
-- stop it with 'stopToolJob').
foreign import javascript safe
  "const m = __exports.memory.buffer; return await globalThis.fractalstreamWorkers.runTool($1, new Uint8Array(m, $2, $3).slice(), new Uint8Array(m, $4, $5).slice(), $6, $7, new Int32Array(m, $8, 2 * $9).slice())"
  runToolOnWorker :: Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Int -> Int -> Ptr Int32 -> Int -> IO JSVal

-- | Stop the tool job with this tag (its worker is replaced: see
-- @js/worker-pool.mjs@); its 'runToolOnWorker' fails.
foreign import javascript unsafe "globalThis.fractalstreamWorkers.stop($1)"
  stopToolJob :: Int -> IO ()

foreign import javascript unsafe "$1.status"
  toolStatus :: JSVal -> IO Int32

foreign import javascript unsafe "$1.arenaBase"
  toolArenaBase :: JSVal -> IO Int

foreign import javascript unsafe "$1.lists.length"
  toolListsLength :: JSVal -> IO Int

foreign import javascript unsafe "$1.draws.length"
  toolDrawsLength :: JSVal -> IO Int

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer, $2, $1.args.length).set($1.args)"
  copyToolArgs :: JSVal -> Ptr Word8 -> IO ()

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer, $2, $1.lists.length).set($1.lists)"
  copyToolLists :: JSVal -> Ptr Word8 -> IO ()

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer, $2, $1.draws.length).set($1.draws)"
  copyToolDraws :: JSVal -> Ptr Word8 -> IO ()

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
  "const m = __exports.memory.buffer; return await globalThis.fractalstreamWorkers.run($1, new Uint8Array(m, $2, $3).slice(), new Uint8Array(m, $4, $5).slice(), $6, $7, $8, $9, $10, $11, $12, new Int32Array(m, $13, 2 * $14).slice())"
  runOnWorker :: Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Int32 -> Int32 -> Int32
              -> Double -> Double -> Double -> Double -> Ptr Int32 -> Int -> IO JSVal

-- | Whether the pool has a copy of the table with this version.
foreign import javascript unsafe "globalThis.fractalstreamWorkers.hasTable($1)"
  poolHasTable :: Int -> IO Bool

-- | Give the pool a copy of a table's contents (its version's), to send
-- to the workers that need it.
foreign import javascript unsafe
  "globalThis.fractalstreamWorkers.addTable($1, new Uint8Array(__exports.memory.buffer, $2, $3).slice())"
  poolAddTable :: Int -> Ptr Word8 -> Int -> IO ()

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
          pure $ (,) (const workerArgsBase) $ \args argsLen tables misses ViewerArgs{..} -> do
            (snapshot, snapshotLen) <- autocolorSnapshot lookupAutocolor
            result <- withTableList tables $ \tp tn ->
                        runOnWorker kernel args argsLen snapshot snapshotLen
                          vaWidth vaHeight vaSubsamples
                          (fst vaPoint) (snd vaPoint) (fst vaStep) (snd vaStep) tp tn
                      >>= evaluate
            copyPixels result vaBuffer
            copyMisses result misses missCapacity
            resultIssues result
        else do
          inst <- withArrayLen bytes $ \n p -> instantiate p n
          -- (one block at a time on this thread, so one arena will do)
          arena <- mallocForeignPtrBytes arenaSize
          pure $ (,) (fromIntegral . ptrToWordPtr) $ \args _ _ misses ViewerArgs{..} ->
            withForeignPtr arena $ \a -> do
              -- (the snapshot itself: nothing can replace it while the
              -- kernel runs, on this one thread)
              (snapshot, _) <- autocolorSnapshot lookupAutocolor
              runKernel inst vaBuffer vaWidth vaHeight vaSubsamples args a (fromIntegral arenaSize)
                        snapshot misses
                        (fst vaPoint) (snd vaPoint) (fst vaStep) (snd vaStep)
      let used = usedNames code
      action $ ViewerFunction $ \vaArgs -> do
        -- The configuration's values, once for the whole render.
        Args args size tables <- makeArgs argsBase (workers == 0) used vaArgs
        -- A block that met points to add (autocolor) is drawn again, once
        -- they're added, until it meets none: so it comes out as with the
        -- LLVM backend, which adds them as it goes.
        pure $ \block -> (`finally` mapM_ (\(t, _, _) -> touchForeignPtr (tableBuffer t)) tables) $
          withForeignPtr args $ \p ->
          allocaBytes (8 + missSize * missCapacity) $ \misses -> do
            let go :: Int -> IO Int32
                go tries = do
                  pokeByteOff misses 0 (0 :: Int32)
                  pokeByteOff misses 4 (fromIntegral missCapacity :: Int32)
                  issues <- runOnce p size tables misses block
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

-- | A job's arguments: the buffer, its size, and the tables in it, each
-- with where its descriptor is (from the buffer's start) and how many
-- bytes its elements take.
data Args = Args (ForeignPtr Word8) Int [(Table, Int, Int)]

-- | The names of the variables some code reads or sets ('usedVarsInCode'
-- has the ones it reads, and the tables it writes; a tool's write-back
-- needs the others it sets too).
usedNames :: Code env -> Set String
usedNames c = execState (usedVarsInCode c >> indexedFoldM @NoResult assigned c) Set.empty
  where
    assigned :: CodeF NoResult env' -> State (Set String) ()
    assigned = \case
      Set _ name _ -> modify' (Set.insert (symbolVal name))
      _ -> pure ()

data NoResult :: Environment -> Exp Type
type instance Eval (NoResult _) = ()

-- | Lay out a context's values in an arguments buffer (see 'argSize'),
-- with the heap after the slots: lists' nodes, texts' nodes, and tables'
-- descriptors. What the code doesn't use is left out (zeros: an empty
-- list, an empty text, no table). A table's elements are where its buffer
-- is, on this thread (@here@), or else 0, for the worker to fill in.
makeArgs :: (Ptr Word8 -> Int) -> Bool -> Set String -> Context HaskellValue env -> IO Args
makeArgs argsBase here used ctx = do
  let values = fromContext (\name ty v -> (symbolVal name `Set.member` used, SomeArg ty v)) ctx
      slots = sum [ fromMaybe 0 (argSize ty) | (_, SomeArg ty _) <- values ]
      size = slots + sum [ valueListBytes ty v | (True, SomeArg ty v) <- values ]
  args <- mallocForeignPtrBytes (max 8 size)
  tables <- newIORef []
  withForeignPtr args $ \p -> do
    fillBytes p 0 (max 8 size)
    heap <- newIORef slots
    let base = argsBase p
        elems t = if here then fromIntegral (ptrToWordPtr (unsafeForeignPtrToPtr (tableBuffer t))) else 0
        noteTable t off len = modifyIORef' tables ((t, off, len) :)
    foldM_ (\off (u, SomeArg ty v) ->
              if u then pokeArg p base heap (elems, noteTable) off ty v
                   else pure (off + fromMaybe 0 (argSize ty))) 0 values
  Args args size . reverse <$> readIORef tables

-- | The tables of a job, as (version, descriptor offset) pairs for the
-- pool (after making sure it has their contents).
withTableList :: [(Table, Int, Int)] -> (Ptr Int32 -> Int -> IO a) -> IO a
withTableList tables act = do
  forM_ tables $ \(t, _, len) -> do
    has <- poolHasTable (tableVersion t)
    unless has $ withForeignPtr (tableBuffer t) $ \b -> poolAddTable (tableVersion t) (castPtr b) len
  withArrayLen (concat [ [fromIntegral (tableVersion t), fromIntegral off] | (t, off, _) <- tables ]) $ \n p ->
    act p (n `div` 2)

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

-- | How many bytes the nodes of a value's lists and texts take (see
-- "Backend.Wasm.List" and "Backend.Wasm.Text"), if it has any.
valueListBytes :: TypeProxy t -> HaskellType t -> Int
valueListBytes t v = case t of
  ListType i -> fromMaybe 0 (nodeSize i) * length v + sum (map (valueListBytes i) v)
  PairType a b -> let (x, y) = v in valueListBytes a x + valueListBytes b y
  TextType -> literalNodeBytes v
  TableType{} -> descriptorBytes (length (tableDims v))
  _ -> 0

-- | Write a value into the arguments buffer at @p@ as 'argSize' lays it
-- out, giving the offset after it. A list's nodes go at the heap's next
-- free offset, and the slot holds its address, where the buffer will be
-- once it's at @base@.
pokeArg :: Ptr Word8 -> Int -> IORef Int -> (Table -> Int, Table -> Int -> Int -> IO ()) -> Int -> TypeProxy t
        -> HaskellType t -> IO Int
pokeArg p base heap tbl@(elems, noteTable) off t v = case t of
  ListType i -> do
    addr <- pokeList p base heap i v
    pokeByteOff p off (fromIntegral addr :: Int32)
    pure (off + 8)
  PairType a b -> let (x, y) = v in pokeArg p base heap tbl off a x >>= \off' -> pokeArg p base heap tbl off' b y
  -- (a table's descriptor: see "Backend.Wasm.Table")
  TableType _ ety -> do
    let n = length (tableDims v)
        (ox, oy) = tableOrigin v
        (sx, sy) = tableStep v
    start <- atomicModifyIORef' heap (\h -> (h + descriptorBytes n, h))
    pokeByteOff p start (fromIntegral (elems v) :: Int32)
    pokeByteOff p (start + 4) (fromIntegral n :: Int32)
    pokeByteOff p (start + 8) (1 :: Int32)
    pokeByteOff p (start + 16) ox
    pokeByteOff p (start + 24) oy
    pokeByteOff p (start + 32) sx
    pokeByteOff p (start + 40) sy
    forM_ (zip [0 ..] (tableDims v)) $ \(k, d) -> pokeByteOff p (start + 48 + 4 * k) (fromIntegral d :: Int32)
    noteTable v start (fromMaybe 0 (elementSize ety) * product (tableDims v))
    pokeByteOff p off (fromIntegral (base + start) :: Int32)
    pure (off + 8)
  TextType -> do
    addr <- pokeText p base heap v
    pokeByteOff p off (fromIntegral addr :: Int32)
    pure (off + 8)
  _ -> pokeScalar p off t v >> pure (off + fromMaybe 0 (argSize t))

-- | A text's literal node, at the heap's next free offset; gives its
-- address (where the buffer will be once it's at @base@).
pokeText :: Ptr Word8 -> Int -> IORef Int -> String -> IO Int
pokeText p base heap s = do
  let bytes = utf8Bytes s
  start <- atomicModifyIORef' heap (\h -> (h + literalNodeBytes s, h))
  pokeByteOff p start (fromIntegral literalNode :: Int32)
  pokeByteOff p (start + 4) (fromIntegral (length bytes) :: Int32)
  forM_ (zip [0 ..] bytes) $ \(k, b) -> pokeByteOff p (start + 8 + k) b
  pure (base + start)

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
  TextType -> pokeText p base heap v >>= \a -> pokeByteOff p off (fromIntegral a :: Int32)
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

------------------------------------------------------------------------
-- Tools

-- | A tool's handler couldn't be compiled: the tool fails to build.
newtype ToolNotCompiled = ToolNotCompiled String
  deriving Show
instance Exception ToolNotCompiled

-- | Tools' handlers compiled to wasm (see 'compileTool'), each when its
-- tool's dispatcher is built, and run on the host's workers. Without a
-- pool of workers, they're interpreted.
wasmToolRunner :: ToolRunner
wasmToolRunner = ToolRunner $ \lookupAutocolor sink layer ctx script -> workerPoolSize >>= \case
  0 -> runTool defaultToolRunner lookupAutocolor sink layer ctx script
  _ -> handle (\(ToolNotCompiled err) -> pure (Left err))
              (buildHandlerWith (wasmToolExec lookupAutocolor (sink layer)) ctx script)

wasmToolExec :: AutocolorLookup -> DrawSink -> ToolExec
wasmToolExec lookupAutocolor sink envp code0 = do
  let code = transformValues backendRewrites code0
  bytes <- either (throwIO . ToolNotCompiled) pure (compileTool envp code)
  kernel <- withArrayLen bytes (\n p -> addKernel p n)
  let used = usedNames code
  pure $ \ctx -> HandlerRun $ \cancelFlag -> snapshotEventArgs ctx >>= \case
    Nothing -> pure (pure ())
    Just vals -> do
      Args args size tables <- makeArgs (const workerArgsBase) False used vals
      (`finally` mapM_ (\(t, _, _) -> touchForeignPtr (tableBuffer t)) tables) $ withForeignPtr args $ \p -> do
        -- (with a bigger arena, if it runs out; stopped when the flag is
        -- set, by stopping its worker, since compiled code there can't see
        -- the flag)
        tag <- atomicModifyIORef' toolTags (\k -> (k + 1, k))
        let stopped = runCancelled cancelFlag >>= \c -> when c (throwIO HandlerCancelled)
            watch = do
              threadDelay 20000
              c <- runCancelled cancelFlag
              if c then stopToolJob tag else watch
            -- (a run that met points to add to autocolor groups is run
            -- again once they're added, as a viewer's block is drawn again)
            attempt :: Int -> Int -> IO JSVal
            attempt tries capacity = do
              stopped
              (snapshot, snapshotLen) <- autocolorSnapshot lookupAutocolor
              result <- bracket (forkIO watch) killThread $ \_ ->
                try (withTableList tables (runToolOnWorker kernel p size snapshot snapshotLen capacity tag) >>= evaluate) >>= \case
                  Right r -> pure r
                  Left (e :: SomeException) -> stopped >> throwIO e
              status <- toolStatus result
              missed <- allocaBytes (8 + missSize * missCapacity) $ \ms -> copyMisses result ms missCapacity >> readMisses ms
              if | testBit status 0 && capacity < toolArenaMaxCapacity -> attempt tries (2 * capacity)
                 | testBit status 0 -> fail $ concat
                     [ "This tool's lists and drawing used more than "
                     , show (toolArenaMaxCapacity `div` (1024 * 1024))
                     , " MB of scratch memory, so its changes were discarded." ]
                 | testBit status 1 -> fail
                     "A list or table index was out of range, so this tool's changes were discarded."
                 | not (null missed) && tries < maxRedraws -> do
                     forM_ missed $ \(mode, n, x, y) -> autocolorFunction lookupAutocolor mode n x y
                     attempt (tries + 1) capacity
                 | otherwise -> pure result
        result <- attempt 0 toolArenaInitialCapacity
        -- What it left: its arguments, its lists, and its drawing.
        copyToolArgs result p
        arenaBase <- toolArenaBase result
        listsLen <- toolListsLength result
        drawsLen <- toolDrawsLength result
        allocaBytes (max 8 listsLen) $ \lists -> allocaBytes (max 8 drawsLen) $ \draws -> do
          copyToolLists result lists
          copyToolDraws result draws
          -- (an address in the worker's memory, in our copies)
          let at addr
                | addr >= workerArgsBase && addr < workerArgsBase + size = p `plusPtr` (addr - workerArgsBase)
                | otherwise = lists `plusPtr` (addr - arenaBase)
              -- (the configuration's tables, by where their descriptors were)
              tableAt addr = lookup addr [ (workerArgsBase + off, t) | (t, off, _) <- tables ]
          offset <- newIORef 0
          sets <- fromContextM (\name ty (arg, old) -> do
                    off <- readIORef offset
                    writeIORef offset (off + fromMaybe 0 (argSize ty))
                    -- (what it doesn't use, it can't have changed)
                    if not (symbolVal name `Set.member` used) then pure (pure ()) else do
                      new <- peekArg at tableAt (p `plusPtr` off) ty old
                      pure $ case argSetValue arg of
                        Just set | Scalar ty old /= Scalar ty new -> set ty new
                        _ -> pure ()) (zipContext ctx vals)
          drawn <- readDraws at sink draws drawsLen
          pure (sequence_ sets >> sequence_ drawn)

-- | Tags for tools' jobs, to stop them by.
toolTags :: IORef Int
toolTags = unsafePerformIO (newIORef 0)
{-# NOINLINE toolTags #-}

-- | Tools get an arena of their own, as with the LLVM backend: it starts
-- at 1 MB and doubles whenever a run runs out, up to 256 MB.
toolArenaInitialCapacity, toolArenaMaxCapacity :: Int
toolArenaInitialCapacity = 1024 * 1024
toolArenaMaxCapacity = 256 * 1024 * 1024

-- | A tool's draw commands (see 'drawRecord'), as what draws them into
-- the sink: they're put in the arena from the top down, so the first is
-- the last record.
readDraws :: (Int -> Ptr Word8) -> DrawSink -> Ptr Word8 -> Int -> IO [IO ()]
readDraws at DrawSink{..} buf len = forM (reverse [0 .. len `div` drawRecordSize - 1]) $ \k -> do
  let r = buf `plusPtr` (k * drawRecordSize)
      f64 :: Int -> IO Double
      f64 = peekByteOff r
      pt o = (,) <$> f64 o <*> f64 (o + 8)
  tag <- peekByteOff r 0 :: IO Int32
  flag <- (/= (0 :: Int32)) <$> peekByteOff r 4
  color <- (\a b c -> rgbToColor (a, b, c)) <$> peekByteOff r 4 <*> peekByteOff r 5 <*> peekByteOff r 6
  case tag of
    0 -> pure dsClear
    1 -> pure (dsStroke color)
    2 -> pure (dsFill color)
    3 -> dsPoint <$> pt 8
    4 -> dsLine <$> pt 8 <*> pt 24
    5 -> dsCircle flag <$> f64 8 <*> pt 16
    6 -> dsRect flag <$> pt 8 <*> pt 24
    7 -> dsWrite <$> (peekByteOff r 4 >>= \(a :: Int32) -> peekText at (fromIntegral a)) <*> pt 8
    _ -> pure (pure ())

-- | Read a value from the arguments buffer as 'pokeArg' wrote it (the
-- kernel writes it back the same way); @at@ finds a list's nodes from
-- their address.
peekArg :: (Int -> Ptr Word8) -> (Int -> Maybe Table) -> Ptr Word8 -> TypeProxy t -> HaskellType t
        -> IO (HaskellType t)
peekArg at tableAt p t old = case t of
  ListType i -> peekByteOff p 0 >>= \(a :: Int32) -> peekList at i (fromIntegral a)
  TextType -> peekByteOff p 0 >>= \(a :: Int32) -> peekText at (fromIntegral a)
  PairType a b -> let (x, y) = old in
    (,) <$> peekArg at tableAt p a x <*> peekArg at tableAt (p `plusPtr` fromMaybe 0 (argSize a)) b y
  -- A table variable holds a configuration's table (its own, unchanged,
  -- or another's), or one the tool made, in its arena: copied out, as a
  -- new version, frozen (others may now look at it).
  TableType _ ety -> peekByteOff p 0 >>= \(a :: Int32) -> case fromIntegral a of
    0 -> pure old
    addr | Just t' <- tableAt addr -> pure t'
    addr -> do
      let d = at addr
      n <- fromIntegral <$> (peekByteOff d 4 :: IO Int32)
      elems <- fromIntegral <$> (peekByteOff d 0 :: IO Int32)
      origin <- (,) <$> peekByteOff d 16 <*> peekByteOff d 24
      step <- (,) <$> peekByteOff d 32 <*> peekByteOff d 40
      dims <- forM [0 .. n - 1] $ \k -> fromIntegral <$> (peekByteOff d (48 + 4 * k) :: IO Int32)
      newTable ety dims origin step >>= \case
        Nothing -> fail ("internal error: a table of " ++ showType ety ++ " can't be made")
        Just t' -> do
          withForeignPtr (tableBuffer t') $ \buf ->
            copyBytes buf (at elems) (fromMaybe 0 (elementSize ety) * product dims)
          freezeTable t'
          pure t'
  _ -> peekScalar p t

peekList :: (Int -> Ptr Word8) -> TypeProxy t -> Int -> IO [HaskellType t]
peekList _ _ 0 = pure []
peekList at t addr = do
  let node = at addr
  next <- peekByteOff node 0 :: IO Int32
  x <- peekElem at (node `plusPtr` 8) t
  (x :) <$> peekList at t (fromIntegral next)

peekElem :: (Int -> Ptr Word8) -> Ptr Word8 -> TypeProxy t -> IO (HaskellType t)
peekElem at p t = case t of
  ListType i -> peekByteOff p 0 >>= \(a :: Int32) -> peekList at i (fromIntegral a)
  TextType -> peekByteOff p 0 >>= \(a :: Int32) -> peekText at (fromIntegral a)
  PairType a b -> (,) <$> peekElem at p a <*> peekElem at (p `plusPtr` fromMaybe 0 (pairSecond a)) b
  _ -> peekScalar p t

-- | A text, from its node's address (see "Backend.Wasm.Text"): its
-- numbers formatted as the interpreter formats them.
peekText :: (Int -> Ptr Word8) -> Int -> IO String
peekText _ 0 = pure ""
peekText at addr = do
  let n = at addr
  kind <- fromIntegral <$> (peekByteOff n 0 :: IO Int32)
  if | kind == literalNode -> do
         len <- peekByteOff n 4 :: IO Int32
         GHC.peekCStringLen utf8 (castPtr (n `plusPtr` 8), fromIntegral len)
     | kind == numberNode -> do
         tag <- peekByteOff n 4 :: IO Int32
         x <- peekByteOff n 8
         y <- peekByteOff n 16
         pure $ case tag of
           0 -> showValue IntegerType (round x)
           1 -> showValue RealType x
           _ -> showValue ComplexType (x :+ y)
     | kind == concatNode -> do
         count <- peekByteOff n 4 :: IO Int32
         parts <- forM [0 .. fromIntegral count - 1] $ \k -> peekByteOff n (8 + 4 * k)
         concat <$> mapM (\(a :: Int32) -> peekText at (fromIntegral a)) parts
     | otherwise -> fail ("internal error: a text node of kind " ++ show kind)

peekScalar :: Ptr Word8 -> TypeProxy t -> IO (HaskellType t)
peekScalar p t = case t of
  BooleanType -> (/= (0 :: Int32)) <$> peekByteOff p 0
  IntegerType -> fromIntegral <$> (peekByteOff p 0 :: IO Int32)
  RealType -> peekByteOff p 0
  ComplexType -> (:+) <$> peekByteOff p 0 <*> peekByteOff p 8
  ColorType -> (\r g b -> rgbToColor (r, g, b)) <$> peekByteOff p 0 <*> peekByteOff p 1 <*> peekByteOff p 2
  _ -> fail ("internal error: a tool's value of type " ++ showType t ++ " can't be read back")
