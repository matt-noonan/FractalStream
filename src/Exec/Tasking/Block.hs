
module Exec.Tasking.Block ( Block(Block)
                          , dynamics
                          , colorizer
                          , coordToModel
                          , oversample
                          , buffer
                          , foreignBuf
                          , xSize
                          , ySize
                          , fillBlock
                          ) where

import Lang.Numbers
import Exec.Region
import Color.Colorize

import Graphics.Gloss.Data.Color

import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Data.Word

data Block a = Block { dynamics  :: Dynamics a
                     , colorizer :: Colorizer a
                     , coordToModel :: (Double, Double) -> a
                     , oversample :: Bool
                     , buffer :: Ptr Word8
                     , foreignBuf :: ForeignPtr Word8
                     , xSize :: Double
                     , ySize :: Double
                     }

fillBlock :: Block C -> IO ()

fillBlock block = do
    forM_ [(x,y) | y <- [0..ySize block - 1]
                 , x <- [0..xSize block - 1] ] $ \(x,y) -> do

        let subsamples = if oversample block then [0,0.5] else [0]
            samples = [(x + dx, y + dy) | dx <- subsamples, dy <- subsamples]

            theDynamics = runDynamics  $ dynamics block
            colorize    = runColorizer $ colorizer block

            colorCoord = colorize . theDynamics . coordToModel block
            (r,g,b) = averageColor $ map colorCoord samples

            index = round $ 4 * (x + (xSize block) * y)
            buf = buffer block

        pokeByteOff buf (index + 3) r
        pokeByteOff buf (index + 2) g
        pokeByteOff buf (index + 1) b
        pokeByteOff buf (index + 0) (0xff :: Word8)

--
--  Color averaging, for anti-aliased drawing
--

rgbOfColor8 :: Color -> (Word8, Word8, Word8)
rgbOfColor8 c = (to8 r, to8 g, to8 b)
    where (r,g,b,_) = rgbaOfColor c
          to8 :: Float -> Word8
          to8 x = round (x * 255)

averageOver :: Floating b => (a -> b) -> [a] -> b
averageOver f xs = sum $ map ((/ n) . f) xs where n = fromIntegral $ length xs

getRed :: Color -> Float
getRed c = r where (r,_,_,_) = rgbaOfColor c

getGreen :: Color -> Float
getGreen c = g where (_,g,_,_) = rgbaOfColor c

getBlue :: Color -> Float
getBlue c = b where (_,_,b,_) = rgbaOfColor c

averageColor :: [Color] -> (Word8, Word8, Word8)
averageColor cs = rgbOfColor8 $ makeColor r g b 1
    where r = averageOver getRed   cs
          g = averageOver getGreen cs
          b = averageOver getBlue  cs