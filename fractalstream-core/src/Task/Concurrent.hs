{- |
Module      : Task.Concurrent
Description : Utilities for concurrent operations
-}

module Task.Concurrent
       ( forPool
       , forPool_
       , Synchronizable()
       , synchronized
       , synchronizedTo
       , with
       , synchedWith
       ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

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
with obj action = do
  let usrs = users obj
  -- Block until general access is allowed.
  _ <- takeMVar (available obj)

  -- Increment the number of active users, and
  -- flag the resource as not finished if we are
  -- the first worker.
  takeMVar usrs >>= \n -> do
      when (n == 0) $ void $ tryTakeMVar (finished obj)
      putMVar usrs (n + 1)

  putMVar (available obj) ()

  -- Perform the action
  result <- action $ resource obj

  -- Decrement the number of users and, if we were the last
  -- active user, flag the resource as finished.
  takeMVar usrs >>= \n -> do
      when (n == 1) $ void $ tryPutMVar (finished obj) ()
      putMVar usrs (n - 1)

  return result


-- | Execute an action on a synchronized resource while
--   preventing concurrent access to the resource.  synchedWith
--   will wait until any running actions complete before
--   executing the given action, and will prevent new actions
--   from beginning.
synchedWith :: Synchronizable a -> (a -> IO b) -> IO b
synchedWith obj action = do
  -- Prevent anybody else from beginning new work with this resource.
  void $ takeMVar (available obj)

  -- Wait until there are no users of the resource, then run the action.
  readMVar (finished obj)
  result <- action (resource obj)

  -- Allow others to access the resource again.
  putMVar (available obj) ()
  return result
