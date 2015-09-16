module Exec.Tasking.Manager (progressively) where

import Exec.Tasking.Block

import System.Random
import Data.Array.IO
import Control.Monad
import Control.Concurrent

-- Chop up a block into sub-blocks, delegate rendering
-- tasks for sub-blocks, and blit the results back
-- into an image.
progressively :: (Block a -> IO ()) -> (Block a -> IO ())

progressively render block = do

    let height = xSize block
        width  = ySize block
        xBlocks =  width `div` 16
        yBlocks = height `div` 16

    blockIDs <- shuffle [(x,y) | x <- [0..xBlocks - 1], y <- [0..yBlocks - 1]]

    forM_ [-4, -2, 0, logSampleRate block] $ \rate -> do
      forM_ blockIDs $ \(x,y) -> do
        let subblock = block { xSize = 16, ySize = 16, x0 = 16 * x, y0 = 16 * y, logSampleRate = rate }
        _ <- forkIO $ render subblock
        return ()

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- makeArray xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    makeArray :: [a] -> IO (IOArray Int a)
    makeArray = newListArray (1,n)