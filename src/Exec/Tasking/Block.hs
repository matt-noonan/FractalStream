{- |
Module      : Exec.Tasking.Block
Description : Blocks encapsulate the task of rendering a picture of a
              dynamical system to one or more tiles.
-}
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

-- | A Block carries the information required to go from a 
--   runnable dynamical system and choice of color scheme
--   to a buffer filled with the resulting color data.
data Block a = Block { dynamics  :: Dynamics a                -- ^ The dynamical system to run.
                     , colorizer :: Colorizer a               -- ^ The color scheme to use.
                     , coordToModel :: (Double, Double) -> a  -- ^ Conversion function from block
                                                              --   coordinates to model coordinates.
                     , logSampleRate :: Int       -- ^ The rate of over- or under-sampling to use.
                                                  --      * logSampleRate == 0: draw one point per pixel.
                                                  --      * logSampleRate == N < 0: draw 2^-N by 2^-N pixel
                                                  --          blocks per model point.
                                                  --      * logSampleRate == N > 0: subsample each pixel
                                                  --          on a 2^N by 2^N subgrid and average the
                                                  --          results, for a smoother picture.
                     , blockBuffer :: Ptr Word8   -- ^ The pixel buffer to write into.
                     , xStride :: Int             -- ^ The width of the pixel buffer.
                     , x0 :: Int        -- ^ The upper-left x coordinate of this block in the pixel buffer.
                     , y0 :: Int        -- ^ The upper-left y coordinate of this block in the pixel buffer.
                     , xSize :: Int     -- ^ The width of the block.
                     , ySize :: Int     -- ^ The height of the block
                     , shouldRedraw :: MVar ()  -- ^ A variable used to signal that this block is complete
                                                --   and the pixel buffer should be redrawn.
                     }

-- | Create an action describing how to draw the given block.
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
