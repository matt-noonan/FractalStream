{- |
Module      : Exec.Tasking.Manager
Description : Block-execution strategies.
-}
module Exec.Tasking.Manager (progressively) where

import           Exec.Tasking.Block
import           Utils.Concurrent

import           Control.Concurrent
import           Control.Monad
import           Data.Array.IO
import           System.Random

import           Data.Time          (diffUTCTime, getCurrentTime)

-- | Chop up a block into sub-blocks, delegate rendering
--   tasks for sub-blocks, and blit the results back
--   into an image.
progressively :: (Block a -> IO ()) -> (Block a -> IO ())

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
