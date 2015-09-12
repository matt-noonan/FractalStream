
module Color.Colorize ( Colorizer(Colorizer)
                      , runColorizer
                      , checkers
                      ) where

import Exec.Region
import Graphics.Gloss
import Lang.Numbers

newtype Colorizer a = Colorizer (Result a -> Color)

runColorizer :: Colorizer a -> Result a -> Color
runColorizer (Colorizer f) = f

checkers :: Color -> Color -> Colorizer C
checkers colorA colorB = Colorizer f
    where mix n c = let pct = (sqrt $ fromIntegral n) / 10.0 in mixColors pct (1.0 - pct) c (light c)
          f (Result region z n)
            | region == Interior                     = black
            | region == Exterior && imagPart z > 0   = mix n colorA
            | otherwise                              = mix n colorB
