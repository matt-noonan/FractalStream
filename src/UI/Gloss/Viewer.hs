
module UI.Gloss.Viewer (glossView) where 

import Lang.Numbers
import Exec.Region
import Color.Colorize

import Exec.Tasking.Block

import Control.Monad
import Control.Concurrent

import Graphics.Gloss
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Data.Word

glossView :: Dynamics C -> Colorizer C -> IO ()

glossView dyn col = do
    block <- windowBlock dyn col (512,512)
    _ <- forkIO $ fillBlock block
    simulate 
        (InWindow "FractalStream" (512, 512) (10, 10))
        white
        5
        (foreignBuf block)
        (\b -> bitmapOfForeignPtr 512 512 b False)
        (\_ _ b -> b)


windowBlock :: Dynamics C -> Colorizer C -> (Int, Int) -> IO (Block C)
windowBlock dyn col (x,y) = do
  buf <- mallocForeignPtrBytes (x * y * 4)
  withForeignPtr buf $ \ptr -> 
    return Block { dynamics = dyn
                 , colorizer = col
                 , coordToModel = \(u,v) -> C ((u - fromIntegral x / 2) / 160) ((v - fromIntegral y / 2) / 160)
                 , oversample = True
                 , buffer = ptr
                 , foreignBuf = buf
                 , xSize = fromIntegral x
                 , ySize = fromIntegral y
                 } 

