{- |
Module      : Task.Concurrent
Description : Utilities for concurrent operations
-}

module Task.Concurrent
       ( forPool
       , forPool_
       , renderConcurrency
       , setRenderConcurrency
       , defaultRenderConcurrency
       , Synchronizable()
       , synchronized
       , synchronizedTo
       , with
       , synchedWith
       ) where

import FractalStream.Prelude

import           Control.Concurrent
import           Control.Exception (mask, uninterruptibleMask_, finally)
import           Control.Concurrent.Async
import           Data.IORef
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- | How many blocks of a render to work on at once: the number of
-- capabilities, unless a frontend or backend said otherwise. (The wasm
-- backend does: there's one capability, but its threads each wait on a
-- block rendered by a worker of the JavaScript host.)
renderConcurrency :: IO Int
renderConcurrency = readIORef renderConcurrencyRef >>= maybe getNumCapabilities pure

-- | Work on this many blocks of a render at once (see 'renderConcurrency').
setRenderConcurrency :: Int -> IO ()
setRenderConcurrency n = writeIORef renderConcurrencyRef (Just (max 1 n))

-- | Like 'setRenderConcurrency', unless it was already set (as by a
-- frontend's @-j@).
defaultRenderConcurrency :: Int -> IO ()
defaultRenderConcurrency n =
  atomicModifyIORef' renderConcurrencyRef (\m -> (Just (fromMaybe (max 1 n) m), ()))

renderConcurrencyRef :: IORef (Maybe Int)
renderConcurrencyRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE renderConcurrencyRef #-}

-- | forPool n is similar to forM, except the actions
--   are executed concurrently by a pool of n workers.
forPool :: Int -> [a] -> (a -> IO b) -> IO [b]
forPool nSimul xs action = do
  result <- VM.new (length xs)
  forPool_ nSimul (zip [0..] xs) $ \(i,x) -> do
    y <- action x
    VM.unsafeWrite result i y
  V.toList <$> V.unsafeFreeze result

-- | forPool_ n is similar to forM_, except the actions
--   are executed concurrently by a pool of n workers.
forPool_ :: Int -> [a] -> (a -> IO ()) -> IO ()
forPool_ nSimul xs action
  | nSimul < 1 = forPool_ 1 xs action
  | otherwise = do
      todo <- newMVar xs
      let loop = do
            -- pop a work item
            next <- modifyMVar todo $ \case
              []      -> pure ([], Nothing)
              (x:xs') -> pure (xs', Just x)
            -- process the item and loop
            case next of
              Nothing -> pure ()
              Just x  -> action x >> loop
      forConcurrently_ [1..nSimul] (const loop)

-- | A resource on which actions may be performed
--   by many threads concurrently, or serially
--   by waiting for completion of current actions
--   before allowing more actions on the resource
--   to execute.
data Synchronizable a = Synchronizable
  { available :: MVar ()
  , users     :: MVar Int
  , finished  :: MVar ()
  , resource  :: a
  }

-- | Create a new synchronization structure
--   around a shared resource.
synchronized :: a -> IO (Synchronizable a)
synchronized res = do
  _available <- newMVar ()
  _users     <- newMVar 0
  _finished  <- newMVar ()
  return $ Synchronizable { available = _available
                          , users     = _users
                          , finished  = _finished
                          , resource  = res
                          }

-- | Use an existing synchronized resource to
--   control access to another resource.
synchronizedTo :: a -> Synchronizable b -> Synchronizable a
synchronizedTo res obj = obj { resource = res }

-- | Execute an action on a synchronized resource, perhaps
--   concurrently with other threads.
with :: Synchronizable a -> (a -> IO b) -> IO b
with obj action = mask $ \restore -> do
  let usrs = users obj
  -- Block until general access is allowed. (If this is interrupted, we
  -- haven't taken anything yet.)
  _ <- takeMVar (available obj)

  -- Increment the number of active users, and flag the resource as not
  -- finished if we are the first worker. Uninterruptibly: we hold
  -- 'available', and must give it back.
  uninterruptibleMask_ $ do
    takeMVar usrs >>= \n -> do
      when (n == 0) $ void $ tryTakeMVar (finished obj)
      putMVar usrs (n + 1)
    putMVar (available obj) ()

  -- Perform the action; then, however it ended (a render worker is
  -- cancelled on every redraw), decrement the number of users and, if we
  -- were the last active user, flag the resource as finished. A user that
  -- didn't would keep 'synchedWith' waiting forever.
  restore (action (resource obj)) `finally` uninterruptibleMask_ (
    takeMVar usrs >>= \n -> do
      when (n == 1) $ void $ tryPutMVar (finished obj) ()
      putMVar usrs (n - 1))

-- | Execute an action on a synchronized resource while
--   preventing concurrent access to the resource.  synchedWith
--   will wait until any running actions complete before
--   executing the given action, and will prevent new actions
--   from beginning.
synchedWith :: Synchronizable a -> (a -> IO b) -> IO b
synchedWith obj action = mask $ \restore -> do
  -- Prevent anybody else from beginning new work with this resource.
  void $ takeMVar (available obj)

  -- Wait until there are no users of the resource, then run the action;
  -- and allow others to access the resource again however that ended.
  (restore (readMVar (finished obj)) >> restore (action (resource obj)))
    `finally` uninterruptibleMask_ (putMVar (available obj) ())
