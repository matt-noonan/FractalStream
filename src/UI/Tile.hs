
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

data Tile a = Tile { imageRect  :: Rectangle ImagePoint
                   , tileBuffer :: ForeignPtr Word8
                   , threadId :: ThreadId
                   , shouldRedrawTile :: MVar ()
                   }

tileData :: Tile a -> (Int, Int, ForeignPtr Word8)
tileData tile = (floor w, floor h, tileBuffer tile)
    where (w, h) = dimensions $ imageRect tile

ifModified :: Tile a -> IO () -> IO ()
ifModified tile f = do
    redraw <- tryTakeMVar $ shouldRedrawTile tile
    case redraw of
        Nothing -> return ()
        Just _  -> f

renderTile :: Planar a => Dynamics a -> Colorizer a -> (Int, Int) -> Rectangle a -> IO (Tile a)

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
