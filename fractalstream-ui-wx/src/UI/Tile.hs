{- |
Module      : UI.Tile
Description : Creation and execution of viewer tiles.
-}
module UI.Tile ( Tile()
               , renderTile
               , cancelTile
               , tileFinished
               , tileComplete
               , tileCovered
               , tileRect
               , ifModified
               , ifElseModified
               , withSynchedTileBuffer
               ) where

import FractalStream.Prelude

import Task.Block (subBlockSize)
import Task.Concurrent
import Actor.Viewer (ViewerArgs, ViewerIssues, Progressive(..), ImageTarget(..), renderProgressivelyInto)
import Data.Planar

import Data.Color

import Control.Concurrent
import Control.Concurrent.Async
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Utils (copyBytes, fillBytes)

newtype ImagePoint = ImagePoint (Double,Double)

instance Planar ImagePoint where
    toCoords (ImagePoint (x,y)) = (x, y)
    fromCoords = ImagePoint

-- | A tile in the image viewer.
data Tile = Tile
    { imageRect        :: Rectangle ImagePoint
      -- ^ The region in view space described by the tile.
    , tileBuffer       :: Synchronizable (ForeignPtr Word8)
      -- ^ The buffer into which the tile will draw.
    , tileWorker       :: Async ()
      -- ^ The worker thread which is drawing this tile.
    , shouldRedrawTile :: MVar ()
      -- ^ A value which signals that the tile needs to be redrawn.
    , coveredTile      :: MVar ()
      -- ^ Full once every part of the tile shows something (the seed's
      --   picture, or the render's first pass).
    , completeTile     :: IORef Bool
      -- ^ Whether its render was done (not stopped for a newer one).
    }

-- | Was the tile's render done, all of it (not stopped for a newer one, or
-- cancelled)?
tileComplete :: Tile -> IO Bool
tileComplete = readIORef . completeTile


-- | Cancel the tile, but don't wait for it to finish
cancelTile :: Tile -> IO ()
cancelTile = void . forkIO . cancel . tileWorker

-- | Has the tile finished drawing (or been cancelled)?
tileFinished :: Tile -> IO Bool
tileFinished = fmap isJust . poll . tileWorker

withSynchedTileBuffer :: Tile -> (Ptr Word8 -> IO b) -> IO b
withSynchedTileBuffer tile action = synchedWith (tileBuffer tile) (`withForeignPtr` action)

-- | Does every part of the tile show something yet (the seed's picture,
-- or at least the render's coarsest pass)? So it's worth showing, rather
-- than the last tile, which a newer render's can replace at once. (A
-- tile that has finished is covered.)
tileCovered :: Tile -> IO Bool
tileCovered tile = (||) <$> (not <$> isEmptyMVar (coveredTile tile)) <*> tileFinished tile

-- | Unpack the width and height of a tile.
tileRect :: Tile -> (Int, Int)
tileRect tile = (floor w, floor h)
    where (w, h) = dimensions $ imageRect tile

-- | Perform an action, but only if the tile needs to be redrawn.
ifModified :: Tile -> IO () -> IO ()
ifModified tile f = do
    redraw <- tryTakeMVar $ shouldRedrawTile tile
    case redraw of
        Nothing -> return ()
        Just _  -> f

-- | Perform an action, but only if the tile needs to be redrawn.
-- Otherwise, perform a fallback action
ifElseModified :: Tile -> IO a -> IO a -> IO a
ifElseModified tile yes no = do
    redraw <- tryTakeMVar $ shouldRedrawTile tile
    case redraw of
        Nothing -> no
        Just _  -> yes

-- | Construct a tile from a viewer's block function, and begin drawing to
-- it, progressively ('Actor.Viewer.renderProgressivelyInto', as the web
-- app does, in 'subBlockSize' pieces).
renderTile :: Bool -- ^ Use smoothing?
           -> Maybe (Ptr Word8 -> Ptr Word8 -> IO (Int -> Int -> Bool))
              -- ^ What the tile shows until it's drawn: fill the
              --   (RGB8, row by row) buffer with it, saying which of its
              --   'subBlockSize' pieces it filled; or grey without one.
              --   It's also given a byte per pixel (zeroed, row by row)
              --   to mark the pixels it gave their final color (nonzero),
              --   which the coarse passes then leave alone.
           -> (ViewerArgs -> IO ViewerIssues) -- ^ The viewer's block function
           -> IO Bool      -- ^ Whether the render's no longer wanted (a
                           --   newer one has replaced it): it stops, between
                           --   pieces
           -> (Int, Int)   -- ^ The width and height of this tile.
           -> ((Double, Double), (Double, Double))
              -- ^ The view: its center, and a pixel's width and height.
           -> IO Tile      -- ^ An action which allocates the tile and
                           --   forks a task which draws into it.

renderTile smooth seed fn stale (width, height) (center, (px, py)) = do

    -- Allocate an red/green/blue pixel byte for each point in the tile
    buf <- mallocForeignPtrBytes (3 * width * height)

    -- Initial fill of the image: nothing drawn yet (one row, then copies
    -- of it, which is quick)
    withForeignPtr buf $ \ptr -> when (width > 0 && height > 0) $ do
      forM_ [0 .. width - 1] $ \index -> pokeColor ptr index grey
      forM_ [1 .. height - 1] $ \row ->
        copyBytes (ptr `plusPtr` (3 * width * row)) ptr (3 * width)

    let iRect = rectangle (ImagePoint (0,0))
                          (ImagePoint (fromIntegral width, fromIntegral height))

    -- (which pixels the seed gave their final color, if there's a seed)
    finals <- forM seed $ \_ -> do
      m <- mallocForeignPtrBytes (width * height)
      withForeignPtr m $ \p -> fillBytes p 0 (width * height)
      pure m

    redraw     <- newMVar ()  -- used to request a redraw
    covered    <- newEmptyMVar
    complete   <- newIORef False
    managedBuf <- synchronized buf

    -- The seed (if any) is worked out by the worker, before it renders,
    -- so that starting a render doesn't hold up the caller.
    worker <- async $ do
      shown <- case (seed, finals) of
        (Just fill, Just m) -> with managedBuf (\b -> withForeignPtr b $ \p -> withForeignPtr m (fill p))
                                 <* tryPutMVar redraw ()
        _ -> pure (\_ _ -> False)
      concurrency <- subtract 1 <$> renderConcurrency
      done <- renderProgressivelyInto Progressive
        { prPieceSize = subBlockSize, prConcurrency = concurrency, prScale = 1
        , prSmooth = smooth, prShown = shown, prStale = stale
        , prStarted = const (pure ()), prCovered = void (tryPutMVar covered ())
        , prDrawn = const (void (tryPutMVar redraw ())) }
        ImageTarget { itWidth = width, itHeight = height, itCenter = center
                    , itPixelSize = px, itAspect = py / px
                    , itWith = \act -> with managedBuf (`withForeignPtr` act)
                    , itFinals = finals }
        fn
      writeIORef complete (isJust done)
    link worker

    return Tile { imageRect = iRect
                , tileBuffer = managedBuf
                , tileWorker = worker
                , shouldRedrawTile = redraw
                , coveredTile = covered
                , completeTile = complete
                }
