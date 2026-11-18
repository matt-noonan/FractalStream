{-# LANGUAGE CPP, RecordWildCards #-}
-- | The desktop's GPU, for the GPU backends ("Backend.WGSL"): wgpu-native
-- (WebGPU's C API, @webgpu.h@), through @cbits/fsgpu.c@, as
-- "Backend.Wasm.Gpu" is the web's. On macOS, Linux and Windows:
-- elsewhere there's none ('nativeGpu' is 'Nothing'), and the GPU backends
-- aren't offered.
module Backend.WGPU
  ( nativeGpu
  ) where

import Backend.WGSL.Device

#if defined(darwin_HOST_OS) || defined(linux_HOST_OS) || defined(mingw32_HOST_OS)
import Backend.WGPU.Probe
import Control.Concurrent.MVar
import Control.Monad (forM_)
import Data.Word (Word8, Word32)
import qualified Foreign.Concurrent as FC
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Storable (peek)

data Pipeline
data Buffer

-- (each of these waits for the GPU, so they're safe calls: other threads
-- go on meanwhile)
foreign import ccall safe "fsgpu_init" c_init :: IO CString
foreign import ccall safe "fsgpu_compile" c_compile :: CString -> Ptr CString -> IO (Ptr Pipeline)
foreign import ccall safe "fsgpu_release_pipeline" c_releasePipeline :: Ptr Pipeline -> IO ()
foreign import ccall safe "fsgpu_storage" c_storage :: Word32 -> Ptr CString -> IO (Ptr Buffer)
foreign import ccall safe "fsgpu_write" c_write :: Ptr Buffer -> Word32 -> Ptr Word8 -> Word32 -> IO ()
foreign import ccall safe "fsgpu_release_buffer" c_releaseBuffer :: Ptr Buffer -> IO ()
foreign import ccall safe "fsgpu_run"
  c_run :: Ptr Pipeline -> Ptr Buffer -> Ptr Buffer -> Ptr Word8 -> Word32 -> Word32 -> Ptr Word8 -> Ptr Word32
        -> Ptr Word32 -> Word32 -> Ptr Word8 -> Word32 -> IO CString
foreign import ccall unsafe "fsgpu_free_string" c_freeString :: CString -> IO ()
foreign import ccall unsafe "fsgpu_version" c_version :: IO Word32
foreign import ccall safe "fsgpu_name" c_name :: IO CString

-- | The GPU, started the first time this is run (or why there's none).
-- Shaders are compiled by the probe first ("Backend.WGPU.Probe"), so that
-- a panic in wgpu's compiler doesn't take the program down.
nativeGpu :: Maybe (IO (Either String GpuDevice))
nativeGpu = Just $ isProbe >>= \case
  True -> pure (Left "no GPU in the GPU's probe")
  False -> c_init >>= message >>= \case
    Just why -> pure (Left why)
    Nothing -> do
      version <- c_version
      name <- maybe "" id <$> (c_name >>= message)
      probe <- newProbe (show version ++ " " ++ name)
      -- (the last few sets of tables, by their versions and places, each
      -- in one buffer: a render's arguments keep theirs, so one dropped
      -- here is only freed once nothing draws with it)
      tableSets <- newMVar []
      pure (Right GpuDevice { gdCompile = compile probe tableSets })
  where
    compile probe tableSets source = probeShader probe source >>= \case
      Failed why -> pure (Left why)
      _ -> withCString source $ \src -> alloca $ \errp -> do
        p <- c_compile src errp
        if p == nullPtr then Left . maybe "the GPU wouldn't compile it" id <$> (peek errp >>= message)
        else do
          pipeline <- FC.newForeignPtr p (c_releasePipeline p)
          pure (Right GpuKernel { gkBind = bind tableSets pipeline })

    bind tableSets pipeline ws tables = do
      -- (a binding can't be empty, so there's always a word)
      argsE <- withArrayLen ws $ \n p -> filled (4 * n) [(0, castPtr p, 4 * n)]
      tablesE <- if null tables then pure (Right Nothing) else Just <$$> tableSet tableSets tables
      pure GpuArgs { gaRun = \params w h out colors -> case (argsE, tablesE) of
        (Left why, _) -> pure (Left why)
        (_, Left why) -> pure (Left why)
        (Right args, Right mt) ->
          withForeignPtr pipeline $ \pp -> withForeignPtr args $ \ap -> withMaybe mt $ \tp ->
            alloca $ \issuesp -> do
              -- (the autocolor groups, and room for the points to add, if
              -- the kernel looks colors up)
              let (palette, n, misses) = case colors of
                    Just GpuAutocolor{..} -> (gacPalette, fromIntegral gacWords, gacMisses)
                    Nothing -> (nullPtr, 0, nullPtr)
              e <- c_run pp ap tp params (fromIntegral w) (fromIntegral h) out issuesp
                         palette n misses (fromIntegral missesBytes)
              message e >>= \case
                Just why -> pure (Left why)
                Nothing -> Right <$> peek issuesp }

    tableSet tableSets tables = do
      let key = [ (gtVersion t, gtOffset t) | t <- tables ]
          size = maximum (0 : [ gtOffset t + gtSize t | t <- tables ])
      modifyMVar tableSets $ \sets -> case lookup key sets of
        Just b -> pure (sets, Right b)
        Nothing -> do
          r <- withTables tables $ \parts -> filled size parts
          pure $ case r of
            Left why -> (sets, Left why)
            Right b -> (take keepSets ((key, b) : sets), Right b)
    keepSets = 2

    -- the tables' bytes, while they're needed
    withTables :: [GpuTable] -> ([(Int, Ptr Word8, Int)] -> IO a) -> IO a
    withTables [] k = k []
    withTables (t : ts) k = withForeignPtr (gtBytes t) $ \p ->
      withTables ts (\parts -> k ((gtOffset t, p, gtSize t) : parts))

    -- a storage buffer of this size, with these bytes at these offsets
    filled :: Int -> [(Int, Ptr Word8, Int)] -> IO (Either String (ForeignPtr Buffer))
    filled size parts = alloca $ \errp -> do
      b <- c_storage (fromIntegral size) errp
      if b == nullPtr then Left . maybe "the GPU couldn't make room for it" id <$> (peek errp >>= message)
      else do
        forM_ parts $ \(off, p, n) -> c_write b (fromIntegral off) p (fromIntegral n)
        Right <$> FC.newForeignPtr b (c_releaseBuffer b)

    withMaybe :: Maybe (ForeignPtr Buffer) -> (Ptr Buffer -> IO a) -> IO a
    withMaybe = maybe ($ nullPtr) withForeignPtr

    (<$$>) = fmap . fmap

-- | A message from the C side (which it gave us to free), if there is one.
message :: CString -> IO (Maybe String)
message s | s == nullPtr = pure Nothing
          | otherwise = do
              m <- peekCString s
              c_freeString s
              pure (Just m)
#else
-- | No GPU in this build (one for macOS, Linux or Windows has one).
nativeGpu :: Maybe (IO (Either String GpuDevice))
nativeGpu = Nothing
#endif
