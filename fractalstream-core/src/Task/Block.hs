{-# options_ghc -Wno-type-defaults #-}

{- |
Module      : Task.Block
Description : Blocks encapsulate the task of rendering a picture of a
              dynamical system to one or more tiles.
-}
module Task.Block
  ( Block(..)
  , BlockComputeAction
  , fillBlock
  , progressively
  , progressivelyOver
  , subBlockSize
  ) where

import FractalStream.Prelude

import Task.Concurrent

import Control.Concurrent.MVar
import Foreign.ForeignPtr
import Control.Concurrent
import Data.Array.IO hiding (index)
import System.Random
import Data.Time (diffUTCTime, getCurrentTime)
import Foreign (Ptr, peekByteOff, pokeByteOff, allocaArray)

type BlockComputeAction = Word32 -- ^ Block width, in subsamples
                       -> Word32 -- ^ Block height, in subsamples
                       -> Word32 -- ^ Subsamples per point length
                       -> Complex Double -- ^ Initial (x,y) values
                       -> Complex Double -- ^ (dx,dy) values
                       -> Ptr Word8 -- ^ Output array of colors
                       -> IO ()

-- | A Block carries the information required to go from a
--   runnable dynamical system to a buffer filled with the
--   resulting color data.
data Block =
  Block { coordToModel  :: (Double, Double) -> (Double, Double)
        , compute       :: BlockComputeAction
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
        , deltaX        :: Double
          -- ^ The model size of a pixel in the X direction
        , deltaY        :: Double
          -- ^ The model size of a pixel in the Y direction
        , xSize         :: Int
          -- ^ The width of the block, in subsamples.
        , ySize         :: Int
          -- ^ The height of the block, in subsamples
        , shouldRedraw  :: MVar ()
          -- ^ A variable used to signal that this block is complete
          --   and the pixel buffer should be redrawn.
        , finalPixels   :: Maybe (ForeignPtr Word8)
          -- ^ Which pixels of the buffer already show their final color
          --   (nonzero; one byte per pixel, with the buffer's stride),
          --   such as the background off a globe: a coarse pass leaves
          --   them alone, rather than painting its squares over them.
        }

-- | Render the block into its output buffer.
fillBlock :: Block -> IO ()
fillBlock Block{..} = do
    let skip = if logSampleRate < 0 then 2^(negate logSampleRate) else 1
        k    = if logSampleRate > 0 then 2^logSampleRate          else 1

    let points    = [ (x,y) | y <- [0, skip .. ySize - 1]
                            , x <- [0, skip .. xSize - 1] ]
        indexOf (u,v) = u + v * xStride

    -- Run the computation on each subsampled point. (On a coarse pass,
    -- the points are a square's width apart, not a pixel's.)
    allocaArray (length points * 3 * k * k) $ \tmp -> do
      compute (fromIntegral (xSize `div` skip) * fromIntegral k)
              (fromIntegral (ySize `div` skip) * fromIntegral k)
              (fromIntegral k)
              ((deltaX :+ deltaY) * fromIntegral skip / fromIntegral k)
              (let (u,v) = coordToModel (fromIntegral x0, fromIntegral y0)
               in u :+ v)
              tmp

      -- Resample the results. We'll write back into the same buffer
      -- we started from so that no extra allocation is needed.
      when (k > 1) $ do
        let indexOf' (u,v) = fromIntegral k * u + v * xSize * fromIntegral k^2
            du = [0 .. k - 1]
            dv = [i * xSize * k | i <- [0 .. k - 1]]
            ixOffsets = [u' + v' | u' <- du, v' <- dv]
        forM_ points $ \(u,v) -> do
          let ix = indexOf' (u,v)
              tgt = u + v * xSize
              average :: [Word8] -> Word8
              average = fromIntegral . (`div` k^2) . sum . map fromIntegral
          r <- average <$> mapM (\ix' -> peekByteOff @Word8 tmp (3 * (ix + ix') + 0))
                             ixOffsets
          g <- average <$> mapM (\ix' -> peekByteOff @Word8 tmp (3 * (ix + ix') + 1))
                             ixOffsets
          b <- average <$> mapM (\ix' -> peekByteOff @Word8 tmp (3 * (ix + ix') + 2))
                             ixOffsets
          pokeByteOff tmp (3*tgt + 0) r
          pokeByteOff tmp (3*tgt + 1) g
          pokeByteOff tmp (3*tgt + 2) b

      -- On a coarse pass, a square (a sample's skip x skip pixels)
      -- doesn't cover final pixels, and a square whose sample is a final
      -- pixel (with a different color, as the background off a globe
      -- is) takes its color from another point of its own instead, the
      -- one of its pixels nearest its middle that isn't final. So the
      -- edge of what's final, such as the globe's, is as sharp in the
      -- coarse passes as it will be in the end.
      let coarseFinals = if skip > 1 then finalPixels else Nothing
          square (u, v) = [ (u + du, v + dv) | dv <- [0 .. skip - 1], du <- [0 .. skip - 1] ]
          intPoints = [ (x0 + x, y0 + y) | (x, y) <- points ]
      isFinal <- case coarseFinals of
        Nothing -> pure (\_ -> pure False)
        Just m -> pure (\(u, v) -> withForeignPtr m (\mp -> (/= 0) <$> peekByteOff @Word8 mp (indexOf (u, v))))
      when (isJust coarseFinals) $ forM_ (zip intPoints [0 ..]) $ \(uv, ix) -> do
        sampleFinal <- isFinal uv
        when sampleFinal $ do
          free <- filterM (fmap not . isFinal) (square uv)
          let (cu, cv) = (fromIntegral (fst uv) + fromIntegral skip / 2 - 0.5, fromIntegral (snd uv) + fromIntegral skip / 2 - 0.5) :: (Double, Double)
              dist (u, v) = (fromIntegral u - cu) ^ (2 :: Int) + (fromIntegral v - cv) ^ (2 :: Int)
          case sortOn dist free of
            [] -> pure ()
            (u, v) : _ -> allocaArray 3 $ \one -> do
              compute 1 1 1 (deltaX :+ deltaY)
                      (let (mu, mv) = coordToModel (fromIntegral u, fromIntegral v) in mu :+ mv)
                      one
              forM_ [0, 1, 2] $ \c -> peekByteOff @Word8 one c >>= pokeByteOff tmp (3 * ix + c)

      -- Fill target buffer with result colors.
      with blockBuffer $ \buffer -> withForeignPtr buffer $ \ptr -> do
        forM_ (zip intPoints [0..]) $ \(uv, ix) -> do
          r <- peekByteOff @Word8 tmp (3*ix + 0)
          g <- peekByteOff @Word8 tmp (3*ix + 1)
          b <- peekByteOff @Word8 tmp (3*ix + 2)
          forM_ (square uv) $ \pixel -> do
            final <- isFinal pixel
            unless final $ do
              let offset = indexOf pixel
              pokeByteOff ptr (3*offset + 0) r
              pokeByteOff ptr (3*offset + 1) g
              pokeByteOff ptr (3*offset + 2) b

      -- Completed the block, signal for a redraw
      void (tryPutMVar shouldRedraw ())

-- | The size of the square pieces 'progressively' renders a block in.
subBlockSize :: Int
subBlockSize = 16

-- | Chop up a block into sub-blocks, delegate rendering
--   tasks for sub-blocks, and blit the results back
--   into an image.
progressively :: (Block -> IO ()) -> (Block -> IO ())
progressively = progressivelyOver (\_ _ -> False) (pure ())

-- | 'progressively', over an image that already shows something (the
-- last render, moved into this view) in the pieces the predicate holds
-- for (by piece, counting 'subBlockSize' squares from the top left).
-- The pieces that show nothing yet get the coarse passes first; the
-- others skip them (the picture they show is already better than big
-- squares) and keep it until they're drawn at full resolution, after the
-- new pieces are; then everything is subsampled.
--
-- Each new piece's first pass is at the coarsest rate that fits it (an
-- edge piece whose sides aren't a multiple of 16 or 4 can't have the
-- coarsest ones), and all the first passes come before anything else, so
-- that once they're done the whole image shows something: then the
-- given action is run (at once, if nothing was new).
progressivelyOver :: (Int -> Int -> Bool) -> IO () -> (Block -> IO ()) -> (Block -> IO ())
progressivelyOver shown covered render block = do

    let width   = xSize block
        height  = ySize block
        xBlocks = let (z,r) = width `divMod` subBlockSize
                  in zip [0 .. z - 1] (repeat subBlockSize) ++
                     [(z,r) | r > 0]
        yBlocks = let (z,r) = height `divMod` subBlockSize
                  in zip [0 .. z - 1] (repeat subBlockSize) ++
                     [(z,r) | r > 0]

    subblocks <- shuffle [(x,y) | x <- xBlocks, y <- yBlocks]

    poolSize <- subtract 1 <$> renderConcurrency

    when False $ do
      caps <- getNumCapabilities
      putStrLn $ show caps ++ " capabilities, pool size " ++ show poolSize ++ " (w=" ++ show (xSize block) ++ ", h=" ++ show (ySize block) ++ ")"

    let rates = filter (<= logSampleRate block) [-4, -2, 0, 1]

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
    when False $ putStrLn $ "***** start @ rates=" ++ show rates
    start <- getCurrentTime

    -- (how many first passes are left)
    firstsLeft <- newMVar (length firsts)
    when (null firsts) covered

    forPool_ poolSize todo $ \(isFirst,rate,(x,w),(y,h)) -> do
      render $ block { xSize = w, ySize = h,
                       x0 = subBlockSize * x, y0 = subBlockSize * y,
                       logSampleRate = rate }
      when isFirst $ do
        remaining <- modifyMVar firstsLeft (\n -> pure (n - 1, n - 1))
        when (remaining == 0) covered

    end <- getCurrentTime
    when False $ do
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
