{- |
Module      : UI.Tile
Description : Creation and execution of viewer tiles.
-}
module UI.Tile ( Tile()
               , renderTile
               , tileData
               , ifModified
               ) where

import Lang.Planar
import Exec.Region
import Exec.Tasking.Block
import Exec.Tasking.Manager

import Color.Color
import Color.Colorize

import Control.Concurrent

import Foreign.ForeignPtr
import Data.Word

newtype ImagePoint = ImagePoint (Double,Double)

instance Planar ImagePoint where
    toCoords (ImagePoint (x,y)) = (x, y)
    fromCoords = ImagePoint

-- | A tile in the image viewer.
data Tile a = Tile
    { imageRect  :: Rectangle ImagePoint   -- ^ The region in view space described by the tile.
    , tileBuffer :: ForeignPtr Word8       -- ^ The buffer into which the tile will draw.
    , threadId :: ThreadId                 -- ^ The id of the thread which is drawing this tile.
    , shouldRedrawTile :: MVar ()          -- ^ A value which signals that the tile needs to be redrawn.
    }

-- | Unpack the width, height, and buffer behind a tile. 
tileData :: Tile a -> (Int, Int, ForeignPtr Word8)
tileData tile = (floor w, floor h, tileBuffer tile)
    where (w, h) = dimensions $ imageRect tile

-- | Perform an action, but only if the tile needs to be redrawn.
ifModified :: Tile a -> IO () -> IO ()
ifModified tile f = do
    redraw <- tryTakeMVar $ shouldRedrawTile tile
    case redraw of
        Nothing -> return ()
        Just _  -> f

-- | Construct a tile from a dynamical system, and begin drawing to it.
renderTile :: Planar a
           => Dynamics a   -- ^ The dynamical system to draw.
           -> Colorizer a  -- ^ The per-region color scheme.
           -> (Int, Int)   -- ^ The height and width of this tile.
           -> Rectangle a  -- ^ The region of the dynamical plane corresponding
                           --   to this tile.
           -> IO (Tile a)  -- ^ An action which allocates the tile and 
                           --   forks a task which draws into it.

renderTile dyn col (width, height) mRect = do

    buf <- mallocForeignPtrBytes (4 * width * height)
    ptr <- withForeignPtr buf return

    -- Initial fill of the image
    sequence_ [ pokeColor ptr index grey | index <- [0 .. width * height - 1] ]

    let iRect = rectangle (ImagePoint (0,0)) (ImagePoint (fromIntegral width, fromIntegral height))

    redraw <- newMVar ()

    tid <- forkIO $ progressively fillBlock $ Block { dynamics = dyn
                      , colorizer = col
                      , coordToModel = convertRect iRect mRect . fromCoords
                      , logSampleRate = 1
                      , blockBuffer = ptr
                      , x0 = 0
                      , y0 = 0
                      , xStride = width
                      , xSize = width
                      , ySize = height
                      , shouldRedraw = redraw
                      }

    return Tile { imageRect = iRect
                , tileBuffer = buf
                , threadId = tid
                , shouldRedrawTile = redraw
                }
