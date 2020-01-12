{-# options_ghc -Wno-type-defaults #-}

{- |
Module      : Exec.Tasking.Block
Description : Blocks encapsulate the task of rendering a picture of a
              dynamical system to one or more tiles.
-}
module Exec.Tasking.Block ( Block(..)
                          , fillBlock
                          ) where

import           Color.Colorize

import           Color.Color

import           Utils.Concurrent

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Word
import           Foreign.Ptr

import           Utilities               (groupsOf)

-- | A Block carries the information required to go from a
--   runnable dynamical system and choice of color scheme
--   to a buffer filled with the resulting color data.
data Block a =
  Block { coordToModel  :: (Double,Double) -> a
        , compute       :: [a] -> IO [Color]
        , logSampleRate :: Int       -- ^ The rate of over- or under-sampling to use.
                           --      * logSampleRate == 0: draw one point per pixel.
                           --      * logSampleRate == N < 0: draw 2^-N by 2^-N pixel
                           --          blocks per model point.
                           --      * logSampleRate == N > 0: subsample each pixel
                           --          on a 2^N by 2^N subgrid and average the
                           --          results, for a smoother picture.
        , blockBuffer   :: Synchronizable (Ptr Word8)   -- ^ The pixel buffer to write into.
        , xStride       :: Int             -- ^ The width of the pixel buffer.
        , x0            :: Int        -- ^ The upper-left x coordinate of this block in the pixel buffer.
        , y0            :: Int        -- ^ The upper-left y coordinate of this block in the pixel buffer.
        , xSize         :: Int     -- ^ The width of the block.
        , ySize         :: Int     -- ^ The height of the block
        , shouldRedraw  :: MVar ()  -- ^ A variable used to signal that this block is complete
                          --   and the pixel buffer should be redrawn.
        }

-- | Create an action describing how to draw the given block.
fillBlock :: Block a -> IO ()

fillBlock block = do
    let rate = logSampleRate block
        skip = if rate < 0 then 2^negate rate else 1
        k    = if rate > 0 then 2^rate        else 1
        subsamples  = [s / k | s <- [0..k-1]]
        nSubsamples = floor (k^2)
        buf = blockBuffer block

    let points    = [ (x,y) | y <- [0, skip .. ySize block - 1]
                            , x <- [0, skip .. xSize block - 1] ]
        uv_points = [ (fromIntegral (x0 block + x), fromIntegral (y0 block + y))
                    | (x,y) <- points ]
        indexOf (u,v) = u + v * xStride block

    let subsample (u,v) = [ (u + du, v + dv) | du <- subsamples, dv <- subsamples ]

    let samples = map (coordToModel block) $ concatMap subsample uv_points

    -- Run the computation on each subsampled point.
    results <- compute block samples

    -- Resample the results
    let rgbs = resampleBy averageColor nSubsamples results

    -- Fill target buffer with result colors.
    -- BUG: the idea was to fill the buffer using "with buf", so that
    --      several threads could be writing to different parts of
    --      the buffer at once.  The viewer would then use synchedWith
    --      when rendering the buffer to screen to ensure that no
    --      incomplete blocks were drawn.
    --      But using "with" instead of "synchedWith" here results in
    --      blocks getting rendered that appear to have raw memory
    --      contents inside (mostly black, with some continguous bands
    --      of color).  When everybody is using synchedWith, this
    --      falls back to acting like a mutex guarding access to the
    --      buffer.
    with buf $ \buffer -> do
      forM_ (zip uv_points rgbs) $ \((u,v), rgb) -> do
        let index = floor $ u + v * (fromIntegral $ xStride block)
        forM_ [indexOf (du,dv) | dv <- [0 .. skip - 1]
                               , du <- [0 .. skip - 1] ] $ \offset -> do
          pokeColor buffer (index + offset) rgb

        -- Completed the block, signal for a redraw
        void $ tryPutMVar (shouldRedraw block) ()

resampleBy :: ([a] -> b) -> Int -> [a] -> [b]
resampleBy f n
  | n > 0     = map f . groupsOf n
  | otherwise = map f . groupsOf 1
