
module Exec.Placeholder (rabbit, mandelbrot) where

import Exec.Haskell
import Lang.Numbers
import Exec.Region

f_C :: C -> C -> C
f_C c z = z^2 + c

f_rabbit :: C -> C
f_rabbit = f_C (-0.122565 + 0.744864 * i)

maxIters :: Int
maxNorm2 :: Double
maxIters = 100
maxNorm2 = 100

-- We're unsure if: (1) z hasn't become large yet, and
--                  (2) the number of iterations is below threshold
unsure :: C -> Int -> Bool
unsure z n = n < maxIters && norm2 z < maxNorm2

escaped :: C -> Int -> Region
escaped _ n | n == maxIters  = Interior
            | otherwise      = Exterior

rabbit :: ComplexDynamics
rabbit = ComplexDynamics { function = f_rabbit
                         , continue = unsure
                         , classify = escaped
                         }

mandelbrot :: ParametricComplexDynamics
mandelbrot = ParametricComplexDynamics { family = f_C
                                       , initialValue = const 0
                                       , continueP = unsure
                                       , classifyP = escaped
                                       }
