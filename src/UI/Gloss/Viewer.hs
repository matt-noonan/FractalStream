
module UI.Gloss.Viewer (glossView) where 

import Lang.Numbers
import Exec.Region
import Color.Colorize

import Control.Monad
import Control.Concurrent

import Graphics.Gloss
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Data.Word

glossView :: Dynamics C -> Colorizer C -> IO ()

glossView dyn col = do
    buf <- mallocForeignPtrBytes (512 * 512 * 4)
    _ <- forkIO $ withForeignPtr buf (fill dyn col)
    simulate 
        (InWindow "FractalStream" (512, 512) (10, 10))
        white
        5
        buf
        (\b -> bitmapOfForeignPtr 512 512 b False)
        (\_ _ b -> b)

fill :: Dynamics C -> Colorizer C -> Ptr Word8 -> IO ()
fill (Dynamics dynamics) (Colorizer colorizer) buf = do
    let steps = [-256..255] :: [Double]
    forM_ [(x,-(y+1)) | y <- steps, x <- steps] $ \(x,y) -> do
        let cs = map (colorizer . dynamics . viewToModel) [(x + dx,y + dy) | dx <- [0,0.5], dy <- [0,0.5]]
        let (r,g,b) = rgbOfColor8 (averageColor cs)
        let index = round $ 4*(x + 256 + 512 * (y + 256))
        pokeByteOff buf (index + 3) r
        pokeByteOff buf (index + 2) g
        pokeByteOff buf (index + 1) b
        pokeByteOff buf (index + 0) (0xff :: Word8)

viewToModel :: (Double, Double) -> C
viewToModel (x, y) = C (x / 160.0) (y / 160.0)


rgbOfColor8 :: Color -> (Word8, Word8, Word8)
rgbOfColor8 c = (to8 r, to8 g, to8 b)
    where (r,g,b,_) = rgbaOfColor c
          to8 :: Float -> Word8
          to8 x = round (x * 255)

--
--  Color averaging, for anti-aliased drawing
--

averageOver :: Floating b => (a -> b) -> [a] -> b
averageOver f xs = sum $ map ((/ n) . f) xs where n = fromIntegral $ length xs

getRed :: Color -> Float
getRed c = r where (r,_,_,_) = rgbaOfColor c

getGreen :: Color -> Float
getGreen c = g where (_,g,_,_) = rgbaOfColor c

getBlue :: Color -> Float
getBlue c = b where (_,_,b,_) = rgbaOfColor c

averageColor :: [Color] -> Color
averageColor cs = makeColor r g b 1
    where r = averageOver getRed   cs
          g = averageOver getGreen cs
          b = averageOver getBlue  cs

