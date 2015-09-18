
module Exec.Haskell where

import Lang.Numbers
import Exec.Region

data ComplexDynamics = ComplexDynamics { function :: C -> C
                                       , continue :: C -> Int -> Bool
                                       , classify :: C -> Int -> Region
                                       }

data ParametricComplexDynamics = ParametricComplexDynamics {
                                         family   :: C -> C -> C
                                       , initialValue :: C -> C
                                       , continueP :: C -> Int -> Bool
                                       , classifyP :: C -> Int -> Region
                                       }

runComplexDynamics :: ComplexDynamics -> Dynamics C

runComplexDynamics dyn = Dynamics $ \z -> classify' $ head $ dropWhile (uncurry $ continue dyn) (trace z)
    where trace :: C -> [(C,Int)]
          trace z = zip (iterate f z) [0..] 
          f = function dyn
          classify' (z, n) = Result ((classify dyn) z n) z n

runParametric :: ParametricComplexDynamics -> Dynamics C
runParametric pdyn = Dynamics $ \c -> classify' $ head $ dropWhile (uncurry $ continueP pdyn) (trace c)
    where trace :: C -> [(C,Int)]
          trace c = zip (iterate (f c) (z0 c)) [0..] 
          f  = family pdyn
          z0 = initialValue pdyn
          classify' (z, n) = Result ((classifyP pdyn) z n) z n