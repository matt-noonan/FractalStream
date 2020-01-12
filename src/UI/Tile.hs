{- |
Module      : UI.Tile
Description : Creation and execution of viewer tiles.
-}
module UI.Tile ( Tile()
               , renderTile
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
    , threadId         :: ThreadId                 -- ^ The id of the thread which is drawing this tile.
    , shouldRedrawTile :: MVar ()          -- ^ A value which signals that the tile needs to be redrawn.
    }

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

    buf <- mallocForeignPtrBytes (4 * width * height)
    ptr <- withForeignPtr buf return

    -- Initial fill of the image
    sequence_ [ pokeColor ptr index grey | index <- [0 .. width * height - 1] ]

    let iRect = rectangle (ImagePoint (0,0)) (ImagePoint (fromIntegral width, fromIntegral height))

    redraw     <- newMVar ()  -- used to request a redraw
    managedPtr <- synchronized ptr

    tid <- forkIO $ progressively fillBlock
                  $ Block { coordToModel = convertRect iRect mRect . fromCoords
                          , compute = renderingAction
                          , logSampleRate = 1
                          , blockBuffer = managedPtr
                          , x0 = 0
                          , y0 = 0
                          , xStride = width
                          , xSize = width
                          , ySize = height
                          , shouldRedraw = redraw
                          }

    return Tile { imageRect = iRect
                , tileBuffer = buf `synchronizedTo` managedPtr
                , threadId = tid
                , shouldRedrawTile = redraw
                }
