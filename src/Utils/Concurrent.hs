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

forPool_ :: Int -> [a] -> (a -> IO b) -> IO ()
forPool_ n xs f = void $ forPool n xs f

-- | A resource on which actions may be performed
--   by many threads concurrently, or serially
--   by waiting for completion of current actions
--   before allowing more actions on the resource
--   to execute.
data Synchronizable a = Synchronizable
  { mutable  :: MVar ()
  , users    :: MVar Int
  , finished :: MVar ()
  , resource :: a
  }

synchronized :: a -> IO (Synchronizable a)
synchronized res = do
  _mutable  <- newMVar ()
  _users    <- newMVar 0
  _finished <- newMVar ()
  return $ Synchronizable { mutable  = _mutable
                          , users    = _users
                          , finished = _finished
                          , resource = res
                          }

synchronizedTo :: a -> Synchronizable b -> Synchronizable a
synchronizedTo res obj = Synchronizable { mutable  = mutable obj
                                        , users    = users obj
                                        , finished = finished obj
                                        , resource = res
                                        }
with :: Synchronizable a -> (a -> IO b) -> IO b
with obj action = do
  void $ takeMVar (mutable obj)
  result <- action (resource obj)
  putMVar (mutable obj) ()
  return result
  
synchedWith :: Synchronizable a -> (a -> IO b) -> IO b
synchedWith = with

with' :: Synchronizable a -> (a -> IO b) -> IO b
with' r action = do
  let usrs = users r
  -- Block until general access is allowed.
  readMVar (mutable r)

  -- Increment the number of active users, and
  -- flag the resource as not finished if we are
  -- the first worker.
  n <- takeMVar usrs
  when (n == 0) $ void $ tryTakeMVar (finished r)
  putMVar usrs (n + 1)
  
  -- Perform the action
  result <- action $ resource r

  -- Decrement the number of users and, if we were the last
  -- active user, flag the resource as finished.
  n <- takeMVar usrs
  when (n == 1) $ void $ tryPutMVar (finished r) ()
  putMVar usrs (n - 1)
  
  return result
  
synchedWith' :: Synchronizable a -> (a -> IO b) -> IO b
synchedWith' obj action = do
  -- Prevent anybody else from beginning to work on this resource.
  void $ takeMVar (mutable obj)
  tid <- myThreadId

  -- Wait until there are no users of the resource, then run the action.
  void $ takeMVar (finished obj)
  result <- action (resource obj)

  -- Allow others to access the resource again.
  putMVar (mutable obj) ()
  return result
