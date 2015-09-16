
module Color.Colorize ( Colorizer(Colorizer)
                      , runColorizer
                      , checker
                      , gradient
                      , solid
                      , Color
                      ) where

import Color.Color

import Exec.Region
import Lang.Planar

newtype Colorizer a = Colorizer (Result a -> Color)

runColorizer :: Colorizer a -> Result a -> Color
runColorizer (Colorizer f) = f

checker :: Planar a => Colorizer a -> Colorizer a -> Colorizer a
checker colorizerA colorizerB = Colorizer f
    where f   (Result Interior _ _) = black
          f r@(Result Exterior p _) = (runColorizer $ if snd (toCoords p) > 0 then colorizerA else colorizerB) r

gradient :: Int -> Colorizer a -> Colorizer a -> Colorizer a

gradient n c1 c2 = if n > 0 then Colorizer f else c1
    where f r@(Result _ _ k) = mixColors (pct (fromIntegral $ k `mod` (n * 2)) (fromIntegral $ 2 * n))
                                         (runColorizer c1 $ r)
                                         (runColorizer c2 $ r)
          pct x y = if x < y / 2 then 2 * x / y else 2 - 2 * x / y

solid :: Color -> Colorizer a
solid = Colorizer . const
