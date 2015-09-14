
module Exec.Tasking.Block ( Block(..)
                          , fillBlock
                          ) where

import Exec.Region
import Color.Colorize

import Graphics.Gloss.Data.Color

import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Data.Word

data Block a = Block { dynamics  :: Dynamics a
                     , colorizer :: Colorizer a
                     , coordToModel :: (Double, Double) -> a
                     , logSampleRate :: Int
                     , blockBuffer :: Ptr Word8
                     , xStride :: Int
                     , x0 :: Int
                     , y0 :: Int
                     , xSize :: Double
                     , ySize :: Double
                     }

fillBlock :: Block a -> IO ()

fillBlock block = do
    let skip = (if logSampleRate block < 0 then 2^(negate $ logSampleRate block) else 1)
    forM_ [(x,y) | y <- [0, (fromIntegral skip) .. ySize block - 1]
                 , x <- [0, (fromIntegral skip) .. xSize block - 1] ] $ \(x,y) -> do

        let k = if logSampleRate block > 0 then 2^logSampleRate block else 1
            subsamples = [s / k | s <- [0..k-1]]
            (u,v) = (x + (fromIntegral $ x0 block), y + (fromIntegral $ y0 block))
            samples = [(u + du, v + dv) | du <- subsamples, dv <- subsamples]

            theDynamics = runDynamics  $ dynamics block
            colorize    = runColorizer $ colorizer block

            colorCoord = colorize . theDynamics . coordToModel block
            (r,g,b) = averageColor $ map colorCoord samples

            index = round $ 4 * (u + v * (fromIntegral $ xStride block))
            buf = blockBuffer block

        forM_ [(u',v') | v' <- [0 .. skip - 1], u' <- [0 .. skip - 1] ] $ \(u',v') -> do
            let index' = index + 4 * (u' + v' * xStride block)
            pokeByteOff buf (index' + 3) r
            pokeByteOff buf (index' + 2) g
            pokeByteOff buf (index' + 1) b
            pokeByteOff buf (index' + 0) (0xff :: Word8)

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