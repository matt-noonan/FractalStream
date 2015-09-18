{- |
Module      : Exec.Tasking.Manager
Description : Block-execution strategies.
-}
module Exec.Tasking.Manager (progressively) where

import Exec.Tasking.Block

import System.Random
import Data.Array.IO
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async

forPool :: Int -> [a] -> (a -> IO b) -> IO [b]
forPool nSimul xs f = do
    sem <- newQSem nSimul
    mapConcurrently (with sem . f) xs
  where with s op = do
            waitQSem s
            r <- op
            signalQSem s
            return r

-- | Chop up a block into sub-blocks, delegate rendering
--   tasks for sub-blocks, and blit the results back
--   into an image.
progressively :: (Block a -> IO ()) -> (Block a -> IO ())

progressively render block = do

    let height = xSize block
        width  = ySize block
        xBlocks =  width `div` 16
        yBlocks = height `div` 16

    blockIDs <- shuffle [(x,y) | x <- [0..xBlocks - 1], y <- [0..yBlocks - 1]]

    poolSize <- getNumCapabilities >>= (return . max 4)
    
    forM_ [-4, -2, 0, logSampleRate block] $ \rate -> do
      forPool poolSize blockIDs $ \(x,y) ->
        render $ block { xSize = 16, ySize = 16, x0 = 16 * x, y0 = 16 * y, logSampleRate = rate }

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