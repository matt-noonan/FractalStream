{-# LANGUAGE ScopedTypeVariables #-}
-- | Compiling shaders safely: wgpu is Rust, and a panic in it (a bug, say,
-- in reading an unusual shader) can't unwind through C, so it aborts the
-- whole process. So each shader the GPU hasn't seen before is first read
-- and checked by wgpu (naga) in a child process, the probe: this program
-- again, with @FRACTALSTREAM_GPU_PROBE=1@, which @cbits/fsgpu.c@ turns
-- into a checking server before @main@ runs. Only once the probe survives
-- a shader is it compiled here; if the probe dies, the shader is declined
-- (and the viewer drawn by the next backend), and a new probe started for
-- the next.
--
-- The probe only checks: making the pipeline (translating the shader for
-- Metal, Vulkan or DirectX 12, and the driver compiling that) is most of
-- a compile's time (on an M5, about 200 ms of a new shader's 202), and
-- doing it twice would double it. So a panic in that translation isn't
-- caught; one in reading and checking the shader is, for about 2 ms.
--
-- The probe starts as the GPU does, in the background, and keeps running,
-- so a new shader doesn't wait for a process's start. And the shaders it
-- passed are remembered, by wgpu's version, the GPU and their text, in a
-- file in the user's cache folder: a viewer opened again isn't probed
-- again.
module Backend.WGPU.Probe
  ( Probe
  , Verdict(..)
  , isProbe
  , newProbe
  , probeShader
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (void)
import Data.Bits (xor, shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.IORef
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word64)
import Numeric (readHex, showHex)
import System.Directory (XdgDirectory(XdgCache), createDirectoryIfMissing, getXdgDirectory)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.FilePath ((</>))
import System.IO
import System.Process
import System.Timeout (timeout)

-- | What the probe made of a shader.
data Verdict
  = Safe            -- ^ it passed (now or before): compile it here
  | Failed String   -- ^ it isn't valid, or it crashed wgpu: why
  | Unguarded       -- ^ there's no probe (it couldn't start): compile it here anyway

data Probe = Probe
  { prTag   :: String               -- ^ wgpu's version and the GPU
  , prKnown :: IORef (Set.Set Word64)
  , prFile  :: IORef (Maybe FilePath)
  , prChild :: MVar ChildState
  }

data ChildState = NotStarted | Running Child | Unavailable

data Child = Child
  { chIn     :: Handle
  , chOut    :: Handle
  , chProc   :: ProcessHandle
  , chErrors :: MVar String       -- ^ what it wrote to stderr, once it's closed
  }

-- | The environment variable that makes this program a probe.
probeVariable :: String
probeVariable = "FRACTALSTREAM_GPU_PROBE"

-- | Whether this process is a probe that somehow got as far as Haskell
-- (its C side didn't take over): then it mustn't start probes itself.
isProbe :: IO Bool
isProbe = maybe False (not . null) <$> lookupEnv probeVariable

-- | A probe for this GPU (@tag@: wgpu's version and the GPU's name),
-- starting in the background.
newProbe :: String -> IO Probe
newProbe tag = do
  pr <- Probe tag <$> newIORef Set.empty <*> newIORef Nothing <*> newMVar NotStarted
  void . forkIO $ do
    loadKnown pr
    modifyMVar_ (prChild pr) start
  pure pr

-- | Have the probe read and check a shader (waiting for it to start, if need be).
probeShader :: Probe -> String -> IO Verdict
probeShader pr source = do
  let key = fnv (prTag pr ++ '\0' : source)
      known = Set.member key <$> readIORef (prKnown pr)
  ok <- known
  if ok then pure Safe else modifyMVar (prChild pr) $ \st -> do
    -- (it may have been remembered meanwhile)
    ok' <- known
    st' <- if ok' then pure st else start st
    if ok' then pure (st', Safe) else case st' of
      Unavailable -> pure (st', Unguarded)
      NotStarted -> pure (st', Unguarded)
      Running child -> ask child source >>= \case
        Just Nothing -> do
          remember pr key
          pure (st', Safe)
        Just (Just why) -> pure (st', Failed why)
        Nothing -> do
          why <- crashed child
          pure (NotStarted, Failed why)

-- | Start the probe, if it isn't running yet (and could start).
start :: ChildState -> IO ChildState
start = \case
  NotStarted -> spawn >>= \case
    Right child -> pure (Running child)
    Left why -> do
      hPutStrLn stderr ("GPU: shaders are compiled without a probe (" ++ why ++ "), so a crash in wgpu would end FractalStream")
      pure Unavailable
  st -> pure st

spawn :: IO (Either String Child)
spawn = do
  r <- try $ do
    exe <- getExecutablePath
    env0 <- getEnvironment
    let env' = (probeVariable, "1") : filter ((/= probeVariable) . fst) env0
    (Just i, Just o, Just e, ph) <- createProcess (proc exe [])
      { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
      , env = Just env' }
    hSetBinaryMode i True
    hSetBinaryMode o True
    errors <- newEmptyMVar
    void . forkIO $ do
      s <- try (hGetContents e >>= \s -> evaluate (length s) >> pure s)
      putMVar errors (either (\(_ :: SomeException) -> "") id s)
    let child = Child i o ph errors
    -- (it answers once it has started the GPU; if it never does, it isn't
    -- a probe: its C side didn't run)
    timeout 20000000 (answer o) >>= \case
      Just (Just Nothing) -> pure (Right child)
      Just (Just (Just why)) -> stop child >> pure (Left ("it found no GPU: " ++ why))
      Just Nothing -> Left <$> crashed child
      Nothing -> stop child >> pure (Left "it didn't answer")
  pure $ case r of
    Left (err :: SomeException) -> Left ("it couldn't start: " ++ show err)
    Right x -> x

stop :: Child -> IO ()
stop child = do
  terminateProcess (chProc child)
  void (try (hClose (chIn child)) :: IO (Either SomeException ()))
  void (waitForProcess (chProc child))

-- | Why the probe died, from what it wrote to stderr (wgpu's panic).
crashed :: Child -> IO String
crashed child = do
  void (try (hClose (chIn child)) :: IO (Either SomeException ()))
  void (waitForProcess (chProc child))
  errs <- readMVar (chErrors child)
  let said = [ l | l <- lines errs, not (null l)
                 , not ("note: run with" `isPrefixOf` l), not ("fatal runtime error" `isInfixOf` l) ]
  pure $ "wgpu crashed reading it"
      ++ (if null said then "" else ": " ++ unwords (take 4 said))

-- | Send a shader; its answer, or Nothing if the probe died.
ask :: Child -> String -> IO (Maybe (Maybe String))
ask child source = do
  let bytes = BL.toStrict (BB.toLazyByteString (BB.stringUtf8 source))
  sent <- try $ do
    BS.hPut (chIn child) (word32 (BS.length bytes) <> bytes)
    hFlush (chIn child)
  case sent of
    Left (_ :: SomeException) -> pure Nothing
    Right () -> answer (chOut child)

-- | An answer: a status byte, then the message's length and bytes.
answer :: Handle -> IO (Maybe (Maybe String))
answer h = do
  r <- try $ do
    header <- BS.hGet h 5
    if BS.length header < 5 then pure Nothing else do
      let n = fromIntegral (foldr (\b acc -> acc `shiftL` 8 .|. fromIntegral b) (0 :: Word64) (BS.unpack (BS.drop 1 header)))
      msg <- BS.hGet h n
      pure $ if BS.length msg < n then Nothing
             else Just (if BS.head header == 0 then Nothing else Just (T.unpack (T.decodeUtf8With (\_ _ -> Just '?') msg)))
  pure (either (\(_ :: SomeException) -> Nothing) id r)

word32 :: Int -> BS.ByteString
word32 n = BS.pack [ fromIntegral ((n `shiftR` (8 * k)) .&. 255) | k <- [0 .. 3] ]

-- ---- the shaders known to be safe ----

knownFile :: IO FilePath
knownFile = do
  dir <- getXdgDirectory XdgCache "fractalstream"
  pure (dir </> "gpu-safe-shaders")

loadKnown :: Probe -> IO ()
loadKnown pr = do
  r <- try $ do
    path <- knownFile
    writeIORef (prFile pr) (Just path)
    s <- readFile path
    let keys = Set.fromList [ k | l <- lines s, [(k, "")] <- [readHex l] ]
    void (evaluate (Set.size keys))
    modifyIORef' (prKnown pr) (Set.union keys)
  case r of
    Left (_ :: SomeException) -> pure ()   -- (none yet)
    Right () -> pure ()

-- | Remember a safe shader (in the file too, if it can be written).
remember :: Probe -> Word64 -> IO ()
remember pr key = do
  modifyIORef' (prKnown pr) (Set.insert key)
  readIORef (prFile pr) >>= \case
    Nothing -> pure ()
    Just path -> do
      r <- try $ do
        dir <- getXdgDirectory XdgCache "fractalstream"
        createDirectoryIfMissing True dir
        appendFile path (showHex key "\n")
      case r of
        Left (_ :: SomeException) -> pure ()
        Right () -> pure ()

-- | FNV-1a, 64 bits, over the characters.
fnv :: String -> Word64
fnv = foldl' (\h c -> (h `xor` fromIntegral (ord c)) * 1099511628211) 14695981039346656037
