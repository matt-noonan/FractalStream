{-# options_ghc -Wno-type-defaults #-}

{- |
Module      : Task.Block
Description : Blocks encapsulate the task of rendering a picture of a
              dynamical system to one or more tiles.
-}
module Task.Block
  ( Block(..)
  , fillBlock
  , progressively
  ) where

import Data.Color

import Task.Concurrent

import Control.Concurrent.MVar
import Control.Monad
import Data.Word
--import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Concurrent
import Data.Array.IO hiding (index)
import System.Random
import Data.Time (diffUTCTime, getCurrentTime)

import Utilities (groupsOf)

-- | A Block carries the information required to go from a
--   runnable dynamical system and choice of color scheme
--   to a buffer filled with the resulting color data.
data Block =
  Block { coordToModel  :: (Double, Double) -> (Double, Double)
        -- , compute       :: Word32 -> Ptr Double -> Ptr Color -> IO ()
        , compute       :: [(Double, Double)] -> IO [Color]
        , logSampleRate :: Int
          -- ^ The rate of over- or under-sampling to use.
          --      * logSampleRate == 0: draw one point per pixel.
          --      * logSampleRate == N < 0: draw 2^-N by 2^-N pixel
          --          blocks per model point.
          --      * logSampleRate == N > 0: subsample each pixel
          --          on a 2^N by 2^N subgrid and average the
          --          results, for a smoother picture.
        , blockBuffer   :: Synchronizable (ForeignPtr Word8)
          -- ^ The pixel buffer to write into.
        , xStride       :: Int
          -- ^ The width of the pixel buffer.
        , x0            :: Int
          -- ^ The upper-left x coordinate of this block in the pixel buffer.
        , y0            :: Int
          -- ^ The upper-left y coordinate of this block in the pixel buffer.
        , xSize         :: Int
          -- ^ The width of the block.
        , ySize         :: Int
          -- ^ The height of the block
        , shouldRedraw  :: MVar ()
          -- ^ A variable used to signal that this block is complete
          --   and the pixel buffer should be redrawn.
        }

-- | Render the block into its output buffer.
fillBlock :: Block -> IO ()
fillBlock Block{..} = do
    let skip = if logSampleRate < 0 then 2^(negate logSampleRate) else 1
        k    = if logSampleRate > 0 then 2^logSampleRate          else 1
        subsamples  = [s / k | s <- [0..k-1]]
        nSubsamples = floor (k^2)

    let points    = [ (x,y) | y <- [0, skip .. ySize - 1]
                            , x <- [0, skip .. xSize - 1] ]
        uv_points = [ (fromIntegral (x0 + x), fromIntegral (y0 + y))
                    | (x,y) <- points ]
        indexOf (u,v) = u + v * xStride

    let subsample (u,v) = [ (u + du, v + dv) | du <- subsamples, dv <- subsamples ]

    let samples = map coordToModel $ concatMap subsample uv_points

    -- Run the computation on each subsampled point.
    results <- compute samples

    -- Resample the results
    let rgbs = resampleBy averageColor nSubsamples results

    -- Fill target buffer with result colors.
    with blockBuffer $ \buffer -> withForeignPtr buffer $ \ptr -> do
      forM_ (zip uv_points rgbs) $ \((u,v), rgb) -> do
        let index = floor $ u + v * fromIntegral xStride
        forM_ [indexOf (du,dv) | dv <- [0 .. skip - 1]
                               , du <- [0 .. skip - 1] ] $ \offset -> do
          pokeColor ptr (index + offset) rgb

    -- Completed the block, signal for a redraw
    void (tryPutMVar shouldRedraw ())

resampleBy :: ([a] -> b) -> Int -> [a] -> [b]
resampleBy f n
  | n > 0     = map f . groupsOf n
  | otherwise = map f . groupsOf 1


-- | Chop up a block into sub-blocks, delegate rendering
--   tasks for sub-blocks, and blit the results back
--   into an image.
progressively :: (Block -> IO ()) -> (Block -> IO ())

progressively render block = do

    let subBlockSize = 8
        width   = xSize block
        height  = ySize block
        xBlocks =  case width `divMod` subBlockSize of
            (z, 0) -> z
            (z, _) -> z -- + 1
        yBlocks = case height `divMod` subBlockSize of
            (z, 0) -> z
            (z, _) -> z -- + 1

    blockIDs <- shuffle [(x,y) | x <- [0..xBlocks - 1], y <- [0..yBlocks - 1]]

    poolSize <- subtract 1 <$> getNumCapabilities
    caps <- getNumCapabilities
    putStrLn $ show caps ++ " capabilities, pool size " ++ show poolSize ++ " (w=" ++ show (xSize block) ++ ", h=" ++ show (ySize block) ++ ")"

    let rates = filter (<= logSampleRate block) [-4, -2, 0, logSampleRate block]

    let todo = [(rate, x, y) | rate <- rates, (x,y) <- blockIDs]
    putStrLn $ "***** start @ rates=" ++ show rates
    start <- getCurrentTime
    forPool_ poolSize todo $ \(rate,x,y) -> do
      render $ block { xSize = subBlockSize, ySize = subBlockSize,
                       x0 = subBlockSize * x, y0 = subBlockSize * y,
                       logSampleRate = rate }
    end <- getCurrentTime
    putStrLn $ "***** " ++ show width ++ " x " ++ show height ++ " @ rates " ++ show rates
               ++ " rendered in " ++ show (diffUTCTime end start)

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- makeArray xs
        forM_ [2..n] $ \i -> do
            j <- randomRIO (1,i)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            writeArray ar i vj
        forM [1..n] (readArray ar)
  where
    n = length xs
    makeArray :: [a] -> IO (IOArray Int a)
    makeArray = newListArray (1,n)
