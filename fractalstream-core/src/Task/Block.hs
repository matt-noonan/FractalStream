{-# options_ghc -Wno-type-defaults #-}

{- |
Module      : Task.Block
Description : The order a picture is rendered in, progressively, in
              pieces (see 'Actor.Viewer.renderProgressivelyInto').
-}
module Task.Block
  ( progressivePlan
  , subBlockSize
  ) where

import FractalStream.Prelude
import Data.Array.IO hiding (index)
import System.Random

-- | The size of the square pieces the desktop app renders in.
subBlockSize :: Int
subBlockSize = 16

-- | The order a progressive render draws an image in (as
-- 'Actor.Viewer.renderProgressivelyInto' does, for both apps): its pieces
-- (each @pieceSize@ square, but for those at the right and bottom edges),
-- each as (whether it's the piece's first pass, the pass's sampling rate,
-- the piece's column (in pieces) and width, its row and height). A rate
-- of -4 or -2 is a coarse pass (a sample a 16 or 4 pixel square), 0 full
-- resolution, and 1 smoothed (four samples a pixel); @sampleRate@ is the
-- finest to go to. @shown@ says which pieces (by column and row) already
-- show something (the last image, moved into this view).
--
-- The pieces that show nothing yet get the coarse passes first; the
-- others skip them (the picture they show is already better than big
-- squares) and keep it until they're drawn at full resolution, after the
-- new pieces are; then everything is smoothed. Each new piece's first
-- pass is at the coarsest rate that fits it (an edge piece whose sides
-- aren't a multiple of 16 or 4 can't have the coarsest ones), and all the
-- first passes come before anything else, so that once they're done the
-- whole image shows something.
progressivePlan :: Int -> Int -> Int -> Int -> (Int -> Int -> Bool)
                -> IO [(Bool, Int, (Int, Int), (Int, Int))]
progressivePlan pieceSize sampleRate width height shown = do

    let xBlocks = let (z,r) = width `divMod` pieceSize
                  in zip [0 .. z - 1] (repeat pieceSize) ++
                     [(z,r) | r > 0]
        yBlocks = let (z,r) = height `divMod` pieceSize
                  in zip [0 .. z - 1] (repeat pieceSize) ++
                     [(z,r) | r > 0]

    subblocks <- shuffle [(x,y) | x <- xBlocks, y <- yBlocks]

    let rates = filter (<= sampleRate) [-4, -2, 0, 1]

    let fits rate (x, y) = rate >= 0 || (snd x `mod` 2^(negate rate) == 0 &&
                                         snd y `mod` 2^(negate rate) == 0)
        isShown (x, y) = shown (fst x) (fst y)
        (fresh, kept) = (filter (not . isShown) subblocks, filter isShown subblocks)
        -- (a new piece's first rate: the coarsest that fits, at most 0)
        firstRate xy = case [ rate | rate <- rates, rate <= 0, fits rate xy ] of
          rate : _ -> rate
          [] -> 0
        firsts = [ (True, firstRate xy, x, y) | xy@(x, y) <- fresh ]
        todo = firsts
            ++ [ (False, rate, x, y) | rate <- rates, rate < 0, xy@(x, y) <- fresh
                                     , fits rate xy, rate > firstRate xy ]
            ++ [ (False, 0, x, y) | 0 `elem` rates, xy@(x, y) <- fresh, firstRate xy < 0 ]
            ++ [ (False, 0, x, y) | 0 `elem` rates, (x, y) <- kept ]
            ++ [ (False, rate, x, y) | rate <- rates, rate > 0, (x, y) <- subblocks ]
    pure todo

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
