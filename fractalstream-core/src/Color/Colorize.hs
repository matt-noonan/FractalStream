{-# options_ghc -Wno-type-defaults #-}

{- |
Module      : Color.Colorize
Description : Functions and combinators for converting iteration results to colors.
-}
module Color.Colorize (
-- * The colorizer type
    Color
  , Colorizer(Colorizer)
  , runColorizer
-- * Basic colorizers
  , rainbow
  , blackInterior
  , solid
  , invert
  , lighter
  , darker
  , checker
-- * Discrete gradients
  , linearGradient
  , cyclicGradient
  , multiGradient
-- * Smoothed gradients
  , smoothedLinearGradient
  , smoothedCyclicGradient
  , smoothedMultiGradient
-- * Smoothing utilities
  , loglogSmoothing
  , smoothedRainbow
  ) where

import           Color.Color

import           Exec.Region
import           Lang.Planar

import qualified Data.Map.Strict as M

-- | A colorizer encapsulates the map from results of an
--   iteration to the color to draw.  You can think of a
--   colorizer as packaging up the color scheme for a
--   given picture.
newtype Colorizer a = Colorizer (Result a -> Color)

-- | Apply a color scheme to an iteration result.
runColorizer :: Colorizer a -> Result a -> Color
runColorizer (Colorizer f) = f

-- | A solid color, no matter what the result was.
solid :: Color -> Colorizer a
solid = Colorizer . const

-- | A cyclic rainbow gradient, in a given number of steps.
rainbow :: Int -> Colorizer a
rainbow = smoothedRainbow (const 0)

-- | A combinator which colors the Interior region black, and otherwise
--   falls back to a provided colorizer.
blackInterior :: Colorizer a -> Colorizer a
blackInterior c = Colorizer f
    where f (Result Interior _ _) = black
          f r                     = (runColorizer c) r

-- | Invert the output of another colorizer.
invert :: Colorizer a -> Colorizer a
invert c = Colorizer $ invertColor . runColorizer c

-- | Make a colorizer's output lighter.
lighter :: Colorizer a -> Colorizer a
lighter c = Colorizer $ light . runColorizer c

-- | Make a colorizer's output darker.
darker :: Colorizer a -> Colorizer a
darker c = Colorizer $ dark . runColorizer c

-- | Given two colorizers, select one based on the sign of the
--   y-coordinate of the last point in the computed trace.
checker :: Planar a => Colorizer a -> Colorizer a -> Colorizer a
checker colorizerA colorizerB = Colorizer f
    where f   (Result Interior _ _) = black
          f r@(Result Exterior p _) = (runColorizer $ if snd (toCoords p) > 0 then colorizerA else colorizerB) r

-- | Create a linear gradient from one colorizer to another, in a given number of steps.
linearGradient :: Planar a => Int -> Colorizer a -> Colorizer a -> Colorizer a
linearGradient = smoothedLinearGradient (const 0)

-- | Create a cyclic gradient from a list of colorizers, in a given number of steps.
cyclicGradient :: Int -> [Colorizer a] -> Colorizer a
cyclicGradient = smoothedCyclicGradient (const 0)

-- | Create a custom gradient.
multiGradient :: Int -> M.Map Double (Colorizer a) -> Colorizer a
multiGradient = smoothedMultiGradient (const 0)

-- | A linear gradient in a fixed number of steps, with a smoothing
--   function to interpolate between steps.
smoothedLinearGradient :: (a -> Double)  -- ^ The smoothing function
                       -> Int            -- ^ The number of steps in the gradient.
                       -> Colorizer a    -- ^ The initial colorizer.
                       -> Colorizer a    -- ^ The final colorizer.
                       -> Colorizer a    -- ^ The produced gradient.
smoothedLinearGradient f n c1 c2 = if n > 0 then Colorizer c else c1
    where c r@(Result _ z  k) =
            mixColors (modOne ((f z + (fromIntegral $ k `mod` n)) / fromIntegral n))
                      (runColorizer c1 $ r)
                      (runColorizer c2 $ r)

-- | A cyclic gradient in a fixed number of steps, with a smoothing
--   function to interpolate between steps.
smoothedCyclicGradient :: (a -> Double) -> Int -> [Colorizer a] -> Colorizer a
smoothedCyclicGradient _ _ [] = solid grey
smoothedCyclicGradient f n cs@(c0:_) =
  let m = M.fromList $ zip [0.0, 1 / fromIntegral (length cs) .. 1.0] (cycle cs)
  in  smoothedMultiGradient f n (M.insert 1.0 c0 m)

-- | A custom gradient in a fixed number of steps, with a smoothing
--   function to interpolate between steps.
smoothedMultiGradient :: (a -> Double) -> Int -> M.Map Double (Colorizer a) -> Colorizer a
smoothedMultiGradient f n colors = if M.null colors then solid grey else Colorizer c
    where c r@(Result _ z k) = mix (M.lookupLE p colors) (M.lookupGE p colors)
            where p = modOne ((f z + fromIntegral (k `mod` n)) / fromIntegral n)
                  mix (Just (p0, Colorizer c0)) (Just (p1, Colorizer c1)) = if p0 == p1
                                                                            then c1 r
                                                                            else mixColors ( (p - p0) / (p1 - p0) ) (c1 r) (c0 r)
                  mix _ _ = grey

-- | A rainbow color scheme with custom smoothing.
smoothedRainbow :: (a -> Double) -> Int -> Colorizer a
smoothedRainbow f n = smoothedCyclicGradient f n $ map solid [red, orange, yellow, green, blue, purple]

-- | Smoothly interpolate the gradient for a degree-n polynomial.  Interpolation is done by
--   adding $1 - \log_n (\frac{1}{2} \log (x^2 + y^2))$ to the gradient index.
loglogSmoothing :: Planar a
                => Int           -- ^ The degree $n$
                -> (a -> Double) -- ^ The smoothing function for polynomials of degree $n$.
loglogSmoothing n = \p -> let (x,y) = toCoords p in 1.0 - (log $ (log $ x^2 + y^2) / 2) / log (fromIntegral n)

modOne :: Double -> Double
modOne x = let x' = snd (properFraction x)
           in if x' >= 0 then x' else 1 + x'
