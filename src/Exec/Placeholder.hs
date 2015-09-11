
module Exec.Placeholder (rabbit) where

import Exec.Haskell
import Lang.Numbers
import Exec.Region

f_rabbit :: C -> C
f_rabbit z = z^2 - 0.122565 + 0.744864 * i

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