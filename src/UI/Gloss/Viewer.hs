
module UI.Gloss.Viewer (glossView) where 

import Lang.Numbers
import Exec.Region
import Color.Colorize

import Exec.Tasking.Block
import Exec.Tasking.Manager

import Graphics.Gloss
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word

glossView :: Dynamics C -> Colorizer C -> IO ()

glossView dyn col = do
    let (width, height) = (512, 512)

    buf <- mallocForeignPtrBytes (4 * width * height)
    ptr <- withForeignPtr buf return
    
    let block = windowBlock dyn col (width, height) ptr
    _ <- progressively fillBlock block

    simulate 
        (InWindow "FractalStream" (width, height) (10, 10))
        white
        5
        buf
        (\b -> bitmapOfForeignPtr width height b False)
        (\_ _ b -> b)


windowBlock :: Dynamics C -> Colorizer C -> (Int, Int) -> Ptr Word8 -> Block C
windowBlock dyn col (x,y) ptr = Block
                 { dynamics = dyn
                 , colorizer = col
                 , coordToModel = \(u,v) -> C ((u - fromIntegral x / 2) / 160) ((v - fromIntegral y / 2) / 160)
                 , oversample = True
                 , buffer = ptr
                 , x0 = 0
                 , y0 = 0
                 , xStride = x
                 , xSize = fromIntegral x
                 , ySize = fromIntegral y
                 } 

