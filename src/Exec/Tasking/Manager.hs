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

    let width   = xSize block
        height  = ySize block
        xBlocks =  case width `divMod` 16 of
            (z, 0) -> z
            (z, _) -> z -- + 1
        yBlocks = case height `divMod` 16 of
            (z, 0) -> z
            (z, _) -> z -- + 1

    blockIDs <- shuffle [(x,y) | x <- [0..xBlocks - 1], y <- [0..yBlocks - 1]]

    poolSize <- subtract 1 <$> getNumCapabilities
    caps <- getNumCapabilities
    putStrLn $ show caps ++ " capabilities, pool size " ++ show poolSize ++ " (w=" ++ show (xSize block) ++ ", h=" ++ show (ySize block) ++ ")"

    let rates = if logSampleRate block > 0
                then [-4, -2, 0, logSampleRate block]
                else [-4, -2, 0]
    forM_ rates $ \rate -> do
      putStrLn $ "***** start @ rate=" ++ show rate
      start <- getCurrentTime
      forPool_ poolSize blockIDs $ \(x,y) -> do
        render $ block { xSize = 16, ySize = 16,
                         x0 = 16 * x, y0 = 16 * y,
                         logSampleRate = rate }
      end <- getCurrentTime
      putStrLn $ "***** " ++ show width ++ " x " ++ show height ++ " @ rate " ++ show rate
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
