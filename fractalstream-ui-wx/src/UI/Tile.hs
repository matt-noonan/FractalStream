{- |
Module      : UI.Tile
Description : Creation and execution of viewer tiles.
-}
module UI.Tile ( Tile()
               , renderTile
               , cancelTile
               , tileRect
               , ifModified
               , ifElseModified
               , withSynchedTileBuffer
               ) where

import Task.Block
import Task.Concurrent
import Data.Planar

import Data.Color

import Control.Concurrent
import Control.Concurrent.Async

import Data.Complex
import Control.Monad (void)
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr

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
    }

cancelTile :: Tile -> IO ()
cancelTile = void . forkIO . cancel . tileWorker -- cancel, but get on with life

withSynchedTileBuffer :: Tile -> (Ptr Word8 -> IO b) -> IO b
withSynchedTileBuffer tile action = synchedWith (tileBuffer tile) (`withForeignPtr` action)

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
renderTile :: (Word32 -> Word32 -> Word32 -> Complex Double -> Complex Double -> Ptr Word8 -> IO ()) -- ([(Double, Double)] -> IO [Color]) -- ^ The rendering action
           -> (Int, Int)   -- ^ The height and width of this tile.
           -> Rectangle (Double, Double)
              -- ^ The region of the dynamical plane corresponding
              --   to this tile.
           -> IO Tile      -- ^ An action which allocates the tile and
                           --   forks a task which draws into it.

renderTile renderingAction (width, height) mRect = do
    putStrLn ("renderTile at w=" ++ show width ++ " h=" ++ show height)

    buf <- mallocForeignPtrBytes (3 * width * height)

    -- Initial fill of the image
    withForeignPtr buf $ \ptr ->
      sequence_ [ pokeColor ptr index grey | index <- [0 .. width * height - 1] ]

    let iRect = rectangle (ImagePoint (0,0))
                          (ImagePoint (fromIntegral width, fromIntegral height))

    redraw     <- newMVar ()  -- used to request a redraw
    managedBuf <- synchronized buf

    let (mRectWidth, mRectHeight) = dimensions mRect

    worker <- async $ progressively fillBlock
                    $ Block { coordToModel = convertRect iRect mRect . fromCoords
                            , compute = renderingAction
                            , logSampleRate = 0
                            , blockBuffer = managedBuf
                            , x0 = 0
                            , y0 = 0
                            , deltaX = mRectWidth / fromIntegral width
                            , deltaY = negate (mRectHeight / fromIntegral height)
                            , xStride = width
                            , xSize = width
                            , ySize = height
                            , shouldRedraw = redraw
                            }
    link worker

    return Tile { imageRect = iRect
                , tileBuffer = managedBuf
                , tileWorker = worker
                , shouldRedrawTile = redraw
                }
