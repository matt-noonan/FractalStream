{- |
Module      : UI.Tile
Description : Creation and execution of viewer tiles.
-}
module UI.Tile ( Tile()
               , renderTile
               , cancelTile
               , tileRect
               , ifModified
               , withSynchedTileBuffer
               ) where

import           Exec.Tasking.Block
import           Exec.Tasking.Manager
import           Lang.Planar

import           Color.Color
import           Utils.Concurrent

import           Control.Concurrent
import           Control.Concurrent.Async

import           Data.Word
import           Foreign.ForeignPtr

newtype ImagePoint = ImagePoint (Double,Double)

instance Planar ImagePoint where
    toCoords (ImagePoint (x,y)) = (x, y)
    fromCoords = ImagePoint

-- | A tile in the image viewer.
data Tile = Tile
    { imageRect        :: Rectangle ImagePoint   -- ^ The region in view space described by the tile.
    , tileBuffer       :: Synchronizable (ForeignPtr Word8)       -- ^ The buffer into which the tile will draw.
    , threadId         :: Async ()               -- ^ The id of the thread which is drawing this tile.
    , shouldRedrawTile :: MVar ()          -- ^ A value which signals that the tile needs to be redrawn.
    }

cancelTile :: Tile -> IO ()
cancelTile tile = cancel (threadId tile)

withSynchedTileBuffer :: Tile -> (ForeignPtr Word8 -> IO b) -> IO b
withSynchedTileBuffer tile action = synchedWith (tileBuffer tile) action

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

-- | Construct a tile from a dynamical system, and begin drawing to it.
renderTile :: Planar a
           => ([a] -> IO [Color]) -- ^ The rendering action
           -> (Int, Int)   -- ^ The height and width of this tile.
           -> Rectangle a  -- ^ The region of the dynamical plane corresponding
                           --   to this tile.
           -> IO Tile      -- ^ An action which allocates the tile and
                           --   forks a task which draws into it.

renderTile renderingAction (width, height) mRect = do
    putStrLn ("renderTile at w=" ++ show width ++ " h=" ++ show height)

    buf <- mallocForeignPtrBytes (3 * width * height)

    -- Initial fill of the image
    withForeignPtr buf $ \ptr ->
      sequence_ [ pokeColor ptr index yellow | index <- [0 .. width * height - 1] ]

    let iRect = rectangle (ImagePoint (0,0)) (ImagePoint (fromIntegral width, fromIntegral height))

    redraw     <- newMVar ()  -- used to request a redraw
    managedBuf <- synchronized buf

    tid <- async  $ (if True then progressively else id) fillBlock
                  $ Block { coordToModel = convertRect iRect mRect . fromCoords
                          , compute = renderingAction
                          , logSampleRate = 1
                          , blockBuffer = managedBuf
                          , x0 = 0
                          , y0 = 0
                          , xStride = width
                          , xSize = width
                          , ySize = height
                          , shouldRedraw = redraw
                          }
    link tid

    return Tile { imageRect = iRect
                , tileBuffer = managedBuf
                , threadId = tid
                , shouldRedrawTile = redraw
                }
