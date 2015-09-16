
module Exec.Tasking.Block ( Block(..)
                          , fillBlock
                          ) where

import Exec.Region
import Color.Colorize

import Color.Color

import Control.Monad
import Control.Concurrent.MVar
import Foreign.Ptr
import Data.Word

data Block a = Block { dynamics  :: Dynamics a
                     , colorizer :: Colorizer a
                     , coordToModel :: (Double, Double) -> a
                     , logSampleRate :: Int
                     , blockBuffer :: Ptr Word8
                     , xStride :: Int
                     , x0 :: Int
                     , y0 :: Int
                     , xSize :: Int
                     , ySize :: Int
                     , shouldRedraw :: MVar ()
                     }

fillBlock :: Block a -> IO ()

fillBlock block = do
    let skip = (if logSampleRate block < 0 then 2^(negate $ logSampleRate block) else 1)
    forM_ [(x,y) | y <- [0, skip .. ySize block - 1]
                 , x <- [0, skip .. xSize block - 1] ] $ \(x,y) -> do

        let k = if logSampleRate block > 0 then 2^logSampleRate block else 1
            subsamples = [s / k | s <- [0..k-1]]
            (u,v) = (fromIntegral $ x0 block + x, fromIntegral $ y0 block + y)
            samples = [(u + du, v + dv) | du <- subsamples, dv <- subsamples]

            theDynamics = runDynamics  $ dynamics block
            colorize    = runColorizer $ colorizer block

            colorCoord = colorize . theDynamics . coordToModel block
            (r,g,b) = colorToRGB $ averageColor $ map colorCoord samples

            index = floor $ (u + v * (fromIntegral $ xStride block))
            buf = blockBuffer block

        forM_ [(u',v') | v' <- [0 .. skip - 1], u' <- [0 .. skip - 1] ] $ \(u',v') -> do
            let index' = index + (u' + v' * xStride block)
            pokeColor buf index' $ rgbToColor (r,g,b)

        -- Completed the block for this resolution, signal for a redraw
        _ <- tryPutMVar (shouldRedraw block) ()
        return ()
