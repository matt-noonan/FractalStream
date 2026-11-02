{-# language CPP #-}
{- |
Module       : UI.SingleInstance
Description  : Hand projects opened from the file manager to a FractalStream that's already running
-}
module UI.SingleInstance
  ( handOffFiles
  , listenForFiles
  ) where

-- On Windows and Linux, opening a project from the file manager starts a
-- new FractalStream with the project on its command line, even when one is
-- already running. So a FractalStream started with files first offers them
-- to one that's already running ('handOffFiles'), and exits if it takes
-- them; and the first one listens for files from later ones
-- ('listenForFiles'). (On macOS, the Finder sends the running app an event
-- instead, which "UI.OpenFiles" handles.)
--
-- They talk over a Unix-domain socket (which Windows 10 and later have
-- too), named for the user, in a folder of their own where there is one:
-- each file's absolute path, one per line, in UTF-8.

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (forever, unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe (fromMaybe)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (getTemporaryDirectory, makeAbsolute, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
#if defined(mingw32_HOST_OS)
import Data.Word (Word32)
#endif

-- | Where a running FractalStream listens: in the user's runtime folder
-- (XDG's, on Linux), or else the temporary folder (which is the user's
-- own on macOS and Windows), named for the user, in case it's shared.
socketPath :: IO FilePath
socketPath = do
  dir <- lookupEnv "XDG_RUNTIME_DIR" >>= maybe getTemporaryDirectory pure
  user <- fromMaybe "user" <$> ((<|>) <$> lookupEnv "USER" <*> lookupEnv "USERNAME")
  pure (dir </> ("fractalstream-" ++ user ++ ".sock"))

-- | Connect to the running FractalStream, if there is one.
withRunningInstance :: (Socket -> IO a) -> IO (Maybe a)
withRunningInstance action
  | not isUnixDomainSocketAvailable = pure Nothing
  | otherwise = do
      path <- socketPath
      result <- try $ bracket (socket AF_UNIX Stream defaultProtocol) close $ \s -> do
        connect s (SockAddrUnix path)
        action s
      pure (either (\(_ :: IOException) -> Nothing) Just result)

-- | Offer files to a FractalStream that's already running. Says whether
-- one took them.
handOffFiles :: [FilePath] -> IO Bool
handOffFiles files = do
  paths <- mapM makeAbsolute files
  -- Windows only lets the program the user is using bring its windows to
  -- the front: that's this one, so it lets the running one do it too.
  allowOthersToComeForward
  maybe False (const True) <$> withRunningInstance (\s -> sendAll s (UTF8.fromString (unlines paths)))

-- | Listen for files from FractalStreams started later, unless another
-- FractalStream already is, and pass them on (from another thread).
listenForFiles :: ([FilePath] -> IO ()) -> IO ()
listenForFiles deliver = when isUnixDomainSocketAvailable $ do
  alreadyListening <- withRunningInstance (\_ -> pure ())
  unless (alreadyListening == Just ()) $ void $ try @IOException $ do
    path <- socketPath
    -- Left over from a FractalStream that's gone, since nothing answered.
    void (try @IOException (removeFile path))
    listener <- socket AF_UNIX Stream defaultProtocol
    bind listener (SockAddrUnix path)
    listen listener 5
    void $ forkIO $ forever $ do
      (conn, _) <- accept listener
      -- (A FractalStream checking whether this one is listening sends
      -- nothing.)
      void $ forkIO $ (do message <- receiveAll conn
                          let paths = filter (not . null) (lines (UTF8.toString message))
                          unless (null paths) (deliver paths))
                      `finally` close conn
  where
    receiveAll conn = do
      chunk <- recv conn 4096
      if BS.null chunk then pure BS.empty else (chunk <>) <$> receiveAll conn

allowOthersToComeForward :: IO ()
#if defined(mingw32_HOST_OS)
allowOthersToComeForward = void (allowSetForegroundWindow asfwAny)
  where asfwAny = maxBound

foreign import ccall unsafe "AllowSetForegroundWindow"
  allowSetForegroundWindow :: Word32 -> IO Bool
#else
allowOthersToComeForward = pure ()
#endif
