
module UI.Tile ( Tile()
               , renderTile
               , drawGlossyTile
               ) where

import Lang.Planar
import Exec.Region
import Exec.Tasking.Block
import Exec.Tasking.Manager

import Color.Colorize

import Control.Concurrent
import Foreign.ForeignPtr
import Data.Word

import Graphics.Gloss

newtype ImagePoint = ImagePoint (Double,Double)

instance Planar ImagePoint where
    toCoords (ImagePoint (x,y)) = (x, y)
    fromCoords = ImagePoint

data Tile a = Tile { imageRect  :: Rectangle ImagePoint
                   , tileBuffer :: ForeignPtr Word8
                   , threadId :: ThreadId
                   , draw :: String -> Picture
                   }

drawGlossyTile :: Tile a -> String -> Picture
drawGlossyTile = draw

renderTile :: Planar a => Dynamics a -> Colorizer a -> (Int, Int) -> Rectangle a -> IO (Tile a)

renderTile dyn col (width, height) mRect = do

    buf <- mallocForeignPtrBytes (4 * width * height)
    ptr <- withForeignPtr buf return

    let iRect = rectangle (ImagePoint (0,0)) (ImagePoint (fromIntegral width, fromIntegral height))

    tid <- forkIO $ progressively fillBlock $ Block { dynamics = dyn
                      , colorizer = col
                      , coordToModel = convertRect iRect mRect . fromCoords
                      , logSampleRate = 1
                      , blockBuffer = ptr
                      , x0 = 0
                      , y0 = 0
                      , xStride = width
                      , xSize = fromIntegral width
                      , ySize = fromIntegral height
                      }
    return Tile { imageRect = iRect
                , tileBuffer = buf
                , threadId = tid
                , draw = const (bitmapOfForeignPtr width height buf False)
                }
