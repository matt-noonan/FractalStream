{- |
Module      : UI.ToolProgress
Description : Running a tool's event handler, with a way to stop it if it's slow.

A tool's handler is worked out on another thread ('HandlerRun'), while the
UI waits for it for a moment. Most handlers finish well within that, and
then it's as if they'd run on the UI thread. One that doesn't gets a modal
window saying it's running, with a spinner and buttons to stop it, or to
stop it and switch back to the Navigate tool. Either way, its effects (its
changes to the tool's variables, and its drawing) are applied on the UI
thread, and only if it finished without an error and wasn't stopped.

The window is modal, so nothing else in the app can be used while the
handler runs; but wx timers still fire, and could start another tool's
handler (an @on timer@ or @on refresh@ one), so only one handler runs at a
time: another one that's asked for meanwhile is skipped.
-}
module UI.ToolProgress
  ( RunOutcome(..)
  , runWithProgress
  ) where

import FractalStream.Prelude hiding (get)

import Actor.Event (HandlerRun(..), HandlerCancelled(..), newCancelFlag, cancelRun)

import Graphics.UI.WX hiding (when, Color, Layout, finally)
import qualified Graphics.UI.WXCore.Events as WX
import Graphics.UI.WXCore.WxcClassesMZ (windowDestroy)

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, cancel, poll, waitCatch, Async)
import Control.Exception (SomeException, fromException, finally)
import Data.IORef
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Text.Printf (printf)

-- | How a tool handler's run went.
data RunOutcome
  = RunFinished (IO ())
    -- ^ it finished: apply its effects (on the UI thread)
  | RunFailed SomeException
    -- ^ it failed, with no effect
  | RunStopped Bool
    -- ^ it was stopped, with no effect; and whether to switch back to the
    -- Navigate tool
  | RunSkipped
    -- ^ another handler was running, so this one didn't run

-- | Whether a tool handler is running, anywhere in the app.
handlerRunning :: IORef Bool
handlerRunning = unsafePerformIO (newIORef False)
{-# NOINLINE handlerRunning #-}

-- | How long to wait for a handler before showing that it's running.
patience :: Int
patience = 250000  -- microseconds

-- | Run a tool's handler (the tool's name is for the window, over the
-- given one), as the module's description says.
runWithProgress :: Window a -> String -> HandlerRun -> IO RunOutcome
runWithProgress owner name run = do
  busy <- atomicModifyIORef' handlerRunning (\b -> (True, b))
  if busy then pure RunSkipped else go `finally` writeIORef handlerRunning False
  where
    go = do
      flag <- newCancelFlag
      worker <- async (startHandlerRun run flag)
      timeout patience (waitCatch worker) >>= \case
        Just result -> pure (outcomeOf result)
        Nothing -> showRunning owner name worker (cancelRun flag)

outcomeOf :: Either SomeException (IO ()) -> RunOutcome
outcomeOf = \case
  Right apply -> RunFinished apply
  Left err | Just HandlerCancelled <- fromException err -> RunStopped False
           | otherwise -> RunFailed err

-- | The modal window shown while a handler runs, until it finishes or is
-- stopped.
showRunning :: Window a -> String -> Async (IO ()) -> IO () -> IO RunOutcome
showRunning owner name worker stopWorker = do
  d <- dialog owner [ text := "Running a tool" ]
  p <- panel d []
  message <- staticText p [ text := "Running the \"" ++ name ++ "\" tool..." ]
  spinner <- staticText p [ text := spinnerFrames !! 0 ++ "  0 s" ]
  stopButton <- button p [ text := "Stop" ]
  navigateButton <- button p [ text := "Stop and switch to Navigate" ]
  set d [ layout := container p $ margin 12 $ column 10
            [ hfill (minsize (sz 360 (-1)) (widget message))
            , widget spinner
            , row 8 [ hglue, widget stopButton, widget navigateButton ] ] ]
  started <- getCurrentTime
  -- Whether the handler has been asked to stop, and if so, whether to
  -- switch to Navigate afterwards.
  stopping <- newIORef Nothing
  let stopRun toNavigate = readIORef stopping >>= \case
        Just _ -> pure ()
        Nothing -> do
          writeIORef stopping (Just toNavigate)
          -- (compiled code stops at its flag; the interpreter, at an
          -- asynchronous exception, which waits for compiled code to
          -- notice its flag: so not from the UI thread)
          stopWorker
          void (forkIO (cancel worker))
          set message [ text := "Stopping the \"" ++ name ++ "\" tool..." ]
          set stopButton [ enabled := False ]
          set navigateButton [ enabled := False ]
  result <- showModal d $ \done -> do
    set stopButton [ on command := stopRun False ]
    set navigateButton [ on command := stopRun True ]
    -- (closing the window stops the handler too; the window goes once
    -- it has stopped)
    WX.windowOnClose d (stopRun False)
    ticker <- timer d [ interval := 100 ]
    ticks <- newIORef (0 :: Int)
    set ticker [ on command := poll worker >>= \case
                   Just r -> set ticker [ enabled := False ] >> done (Just r)
                   Nothing -> do
                     n <- atomicModifyIORef' ticks (\k -> (k + 1, k + 1))
                     now <- getCurrentTime
                     let seconds = floor (realToFrac (diffUTCTime now started) :: Double) :: Int
                     set spinner [ text := printf "%s  %d s" (spinnerFrames !! (n `mod` length spinnerFrames)) seconds ] ]
  void (windowDestroy d)
  readIORef stopping <&> \case
    Just toNavigate -> RunStopped toNavigate
    Nothing -> maybe (RunStopped False) outcomeOf result

spinnerFrames :: [String]
spinnerFrames = map pure "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
