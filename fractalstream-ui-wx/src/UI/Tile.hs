{- |
Module      : UI.Tile
Description : Creation and execution of viewer tiles.
-}
module UI.Tile ( Tile()
               , renderTile
               , cancelTile
               , tileFinished
               , tileCovered
               , tileRect
               , ifModified
               , ifElseModified
               , withSynchedTileBuffer
               , StaleRender(..)
               ) where

import FractalStream.Prelude

import Task.Block
import Task.Concurrent
import Data.Planar

import Data.Color

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (Exception, handle)

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
    }

-- | Thrown by a tile's rendering action, instead of computing a block,
-- once a newer render has replaced the tile's: it stops the tile's
-- render there. (A block whose computation is skipped mustn't be copied
-- into the tile: its scratch buffer was never written, and a newer
-- render may still read this tile's buffer as its seed.)
data StaleRender = StaleRender
  deriving Show

instance Exception StaleRender

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

-- | Construct a tile from a dynamical system, and begin drawing to it.
renderTile :: Bool -- ^ Use smoothing?
           -> Maybe (Ptr Word8 -> Ptr Word8 -> IO (Int -> Int -> Bool))
              -- ^ What the tile shows until it's drawn: fill the
              --   (RGB8, row by row) buffer with it, saying which of its
              --   'subBlockSize' pieces it filled; or grey without one.
              --   It's also given a byte per pixel (zeroed, row by row)
              --   to mark the pixels it gave their final color (nonzero),
              --   which the coarse passes then leave alone.
           -> BlockComputeAction -- ^ The rendering action
           -> (Int, Int)   -- ^ The height and width of this tile.
           -> Rectangle (Double, Double)
              -- ^ The region of the dynamical plane corresponding
              --   to this tile.
           -> IO Tile      -- ^ An action which allocates the tile and
                           --   forks a task which draws into it.

renderTile smooth seed renderingAction (width, height) mRect = do

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
    managedBuf <- synchronized buf

    let (mRectWidth, mRectHeight) = dimensions mRect

    -- The seed (if any) is worked out by the worker, before it renders,
    -- so that starting a render doesn't hold up the caller.
    worker <- async $ do
      shown <- case (seed, finals) of
        (Just fill, Just m) -> with managedBuf (\b -> withForeignPtr b $ \p -> withForeignPtr m (fill p))
                                 <* tryPutMVar redraw ()
        _ -> pure (\_ _ -> False)
      handle (\StaleRender -> pure ()) $ progressivelyOver shown (void (tryPutMVar covered ())) fillBlock
                    $ Block { coordToModel = convertRect iRect mRect . fromCoords
                            , compute = renderingAction
                            , logSampleRate = if smooth then 1 else 0
                            , blockBuffer = managedBuf
                            , x0 = 0
                            , y0 = 0
                            , deltaX = mRectWidth / fromIntegral width
                            , deltaY = negate (mRectHeight / fromIntegral height)
                            , xStride = width
                            , xSize = width
                            , ySize = height
                            , shouldRedraw = redraw
                            , finalPixels = finals
                            }
    link worker

    return Tile { imageRect = iRect
                , tileBuffer = managedBuf
                , tileWorker = worker
                , shouldRedrawTile = redraw
                , coveredTile = covered
                }
