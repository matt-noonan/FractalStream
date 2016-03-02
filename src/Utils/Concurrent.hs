{- |
Module      : Utils.Concurrent
Description : Utilities for concurrent operations
-}

module Utils.Concurrent
       ( forPool
       , forPool_
       , Synchronizable()
       , synchronized
       , synchronizedTo
       , with
       , synchedWith
       ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad

-- | forPool n is similar to forM, except the actions
--   are executed concurrently by a pool of n workers.
forPool :: Int -> [a] -> (a -> IO b) -> IO [b]
forPool nSimul xs f
  | nSimul < 1 = forPool 1 xs f
  | otherwise = do
      sem <- newQSem nSimul
      mapConcurrently (withSem sem . f) xs
  where withSem s op = do
          waitQSem s
          r <- op
          signalQSem s
          return r

-- | forPool_ n is similar to forM_, except the actions
--   are executed concurrently by a pool of n workers.
forPool_ :: Int -> [a] -> (a -> IO b) -> IO ()
forPool_ n xs f = void $ forPool n xs f

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

{- If the functions 'with' and 'synchedWith' are replaced
   with the definitions below, no rendering anomalies occur.

with :: Synchronizable a -> (a -> IO b) -> IO b
with obj action = do
  void $ takeMVar (available obj)
  result <- action (resource obj)
  putMVar (available obj) ()
  return result

synchedWith :: Synchronizable a -> (a -> IO b) -> IO b
synchedWith obj = ($ resource obj)
-}

-- | Execute an action on a synchronized resource, perhaps
--   concurrently with other threads.
with :: Synchronizable a -> (a -> IO b) -> IO b
with obj action = do
  let usrs = users obj
  -- Block until general access is allowed.
  readMVar (available obj)

  -- Increment the number of active users, and
  -- flag the resource as not finished if we are
  -- the first worker.
  n <- takeMVar usrs
  when (n == 0) $ void $ tryTakeMVar (finished obj)
  putMVar usrs (n + 1)
  
  -- Perform the action
  result <- action $ resource obj

  -- Decrement the number of users and, if we were the last
  -- active user, flag the resource as finished.
  n <- takeMVar usrs
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
  void $ takeMVar (finished obj)
  result <- action (resource obj)

  -- Allow others to access the resource again.
  putMVar (available obj) ()
  return result
