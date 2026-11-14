{- |
Module      : UI.Loading
Description : Loading a project on another thread, with a window saying so if it's slow.

Opening a project reads and checks its script and compiles its viewers,
which can take a while for a big one. That's done on another thread
('whileLoading'), while the UI waits for it for a moment. Most projects are
ready well within that, and then it's as if they'd been loaded on the UI
thread. One that isn't gets a window saying it's loading, with a spinner
(which keeps turning, since the UI thread is free) and a Cancel button.

The window is modal, so nothing else in the app can be used meanwhile, and
the caller of 'whileLoading' simply gets the result once it's ready, as if
it had loaded it itself.
-}
module UI.Loading
  ( whileLoading
  ) where

import FractalStream.Prelude hiding (get)
import FractalStream.Quoted (quoted)

import UI.ToolProgress (spinnerFrames)

import Graphics.UI.WX hiding (when, Color, Layout, finally)
import qualified Graphics.UI.WXCore.Events as WX
import Graphics.UI.WXCore.WxcClassesMZ (windowDestroy)

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, cancel, poll, waitCatch, Async)
import Control.Exception (SomeException, throwIO)
import Data.IORef
import Data.Time (getCurrentTime, diffUTCTime)
import System.Timeout (timeout)
import Text.Printf (printf)

-- | How long to wait for a project before showing that it's loading.
patience :: Int
patience = 500000  -- microseconds

-- | Load something (the project of the given name) on another thread, as
-- the module's description says: its result, or 'Nothing' if the user
-- cancelled. An exception it throws is thrown again here, on the UI
-- thread.
whileLoading :: String -> IO a -> IO (Maybe a)
whileLoading name action = do
  worker <- async action
  outcome <- timeout patience (waitCatch worker) >>= \case
    Just result -> pure (Just result)
    Nothing -> showLoading name worker
  traverse (either throwIO pure) outcome

-- | The modal window shown while a project loads, until it's ready or the
-- user cancels (and the loading has stopped).
showLoading :: String -> Async a -> IO (Maybe (Either SomeException a))
showLoading name worker = do
  -- (no parent: the project's own window is hidden until it's loaded,
  -- and wx would show it once a dialog over it closed)
  d <- dialog objectNull [ text := "Opening a project" ]
  p <- panel d []
  message <- staticText p [ text := "Loading " ++ quoted name ++ "..." ]
  spinner <- staticText p [ text := spinnerFrames !! 0 ++ "  0 s" ]
  cancelButton <- button p [ text := "Cancel" ]
  set d [ layout := container p $ margin 12 $ column 10
            [ hfill (minsize (sz 360 (-1)) (widget message))
            , widget spinner
            , row 8 [ hglue, widget cancelButton ] ] ]
  started <- getCurrentTime
  cancelling <- newIORef False
  let cancelLoading = readIORef cancelling >>= \case
        True -> pure ()
        False -> do
          writeIORef cancelling True
          -- (it stops once whatever it's doing gets to a point where it
          -- can: compiling a viewer, say, isn't interrupted; so not
          -- waiting for that here)
          void (forkIO (cancel worker))
          set message [ text := "Cancelling..." ]
          set cancelButton [ enabled := False ]
  result <- showModal d $ \done -> do
    set cancelButton [ on command := cancelLoading ]
    -- (closing the window cancels too; it goes once loading has stopped)
    WX.windowOnClose d cancelLoading
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
  readIORef cancelling <&> \case
    True -> Nothing
    False -> result
