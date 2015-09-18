
module Color.Colorize ( Colorizer(Colorizer)
                      , runColorizer
                      , checker
                      , linearGradient
                      , cyclicGradient
                      , multiGradient
                      , smoothedLinearGradient
                      , smoothedCyclicGradient
                      , smoothedMultiGradient
                      , loglogSmoothing
                      , rainbow
                      , smoothedRainbow
                      , blackInterior
                      , solid
                      , invert
                      , lighter
                      , darker
                      , Color
                      ) where

import Color.Color

import Exec.Region
import Lang.Planar

import qualified Data.Map.Strict as M

newtype Colorizer a = Colorizer (Result a -> Color)

runColorizer :: Colorizer a -> Result a -> Color
runColorizer (Colorizer f) = f

checker :: Planar a => Colorizer a -> Colorizer a -> Colorizer a
checker colorizerA colorizerB = Colorizer f
    where f   (Result Interior _ _) = black
          f r@(Result Exterior p _) = (runColorizer $ if snd (toCoords p) > 0 then colorizerA else colorizerB) r


loglogSmoothing :: Planar a => Int -> (a -> Double)
loglogSmoothing n = \p -> let (x,y) = toCoords p in 1.0 - (log $ (log $ x^2 + y^2) / 2) / log (fromIntegral n)

linearGradient :: Planar a => Int -> Colorizer a -> Colorizer a -> Colorizer a

linearGradient = smoothedLinearGradient (const 0)

smoothedLinearGradient :: (a -> Double) -> Int -> Colorizer a -> Colorizer a -> Colorizer a

smoothedLinearGradient f n c1 c2 = if n > 0 then Colorizer c else c1
    where c r@(Result _ z  k) = mixColors ((f z + (fromIntegral $ k `mod` n)) / fromIntegral n)
                                          (runColorizer c1 $ r)
                                          (runColorizer c2 $ r)

smoothedCyclicGradient :: (a -> Double) -> Int -> [Colorizer a] -> Colorizer a

smoothedCyclicGradient _ _ [] = solid grey
smoothedCyclicGradient f n cs = smoothedMultiGradient f n $ M.fromList $ zip [0.0, 1 / fromIntegral (length cs) .. 1.0] (cycle cs)

cyclicGradient :: Int -> [Colorizer a] -> Colorizer a

cyclicGradient = smoothedCyclicGradient (const 0)

multiGradient :: Int -> M.Map Double (Colorizer a) -> Colorizer a

multiGradient = smoothedMultiGradient (const 0)

smoothedMultiGradient :: (a -> Double) -> Int -> M.Map Double (Colorizer a) -> Colorizer a

smoothedMultiGradient f n colors = if M.null colors then solid grey else Colorizer c
    where c r@(Result _ z k) = mix (M.lookupLE p colors) (M.lookupGE p colors)
            where p = (f z + fromIntegral (k `mod` n)) / fromIntegral n
                  mix (Just (p0, Colorizer c0)) (Just (p1, Colorizer c1)) = if p0 == p1
                                                                            then c1 r
                                                                            else mixColors ( (p - p0) / (p1 - p0) ) (c1 r) (c0 r)
                  mix _ _ = black

rainbow :: Int -> Colorizer a
rainbow = smoothedRainbow (const 0)

smoothedRainbow :: (a -> Double) -> Int -> Colorizer a
smoothedRainbow f n = smoothedCyclicGradient f n $ map solid [red, orange, yellow, green, blue, purple]

solid :: Color -> Colorizer a
solid = Colorizer . const

blackInterior :: Colorizer a -> Colorizer a
blackInterior c = Colorizer f
    where f (Result Interior _ _) = black
          f r = (runColorizer c) r

invert :: Colorizer a -> Colorizer a
invert c = Colorizer $ invertColor . runColorizer c

lighter :: Colorizer a -> Colorizer a
lighter c = Colorizer $ light . runColorizer c

darker :: Colorizer a -> Colorizer a
darker c = Colorizer $ dark . runColorizer c

