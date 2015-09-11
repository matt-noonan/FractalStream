
module Exec.Haskell where

import Lang.Numbers
import Exec.Region

data ComplexDynamics = ComplexDynamics { function :: C -> C
                                       , continue :: C -> Int -> Bool
                                       , classify :: C -> Int -> Region
                                       }

runComplexDynamics :: ComplexDynamics -> Dynamics C

runComplexDynamics dyn = Dynamics $ \z -> classify' $ head $ dropWhile (uncurry $ continue dyn) (trace z)
    where trace :: C -> [(C,Int)]
          trace z = zip (iterate f z) [0..] 
          f = function dyn
          classify' (z, n) = Result ((classify dyn) z n) z n