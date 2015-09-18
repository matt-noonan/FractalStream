{- |
Module      : Exec.Placeholder
Description : Some placeholder dynamical systems, as Haskell code.
-}
module Exec.Placeholder (rabbit, mandelbrot) where

import Exec.Haskell
import Lang.Numbers
import Exec.Region

-- | The family of quadratic polynomials.
f_C :: C -> C -> C
f_C c z = z^2 + c

-- | The Douady rabbit
f_rabbit :: C -> C
f_rabbit = f_C (-0.122565 + 0.744864 * i)

maxIters :: Int
maxNorm2 :: Double
maxIters = 100
maxNorm2 = 100

-- | We're unsure if:
--   * z hasn't become large yet, and
--   * the number of iterations is below threshold
unsure :: C -> Int -> Bool
unsure z n = n < maxIters && norm2 z < maxNorm2

-- | If we hit the maximum number of iterations, we're in the interior.
--   Otherwise, the exterior.
escaped :: C -> Int -> Region
escaped _ n | n == maxIters  = Interior
            | otherwise      = Exterior

-- | A description of the "Douady rabbit" Julia set.
rabbit :: ComplexDynamics
rabbit = ComplexDynamics { function = f_rabbit
                         , continue = unsure
                         , classify = escaped
                         }

-- | A description of the Mandelbrot set.
mandelbrot :: ParametricComplexDynamics
mandelbrot = ParametricComplexDynamics { family = f_C
                                       , initialValue = const 0
                                       , continueP = unsure
                                       , classifyP = escaped
                                       }
