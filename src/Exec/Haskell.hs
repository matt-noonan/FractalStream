{- |
Module      : Exec.Haskell
Description : Dynamical systems built as native Haskell functions
-}
module Exec.Haskell (
-- * Dynamics of one complex variable
      ComplexDynamics(..)
    ,  runComplexDynamics
-- * Complex families of 1-dimensional complex dynamical systems.
    , ParametricComplexDynamics(..)
    , runParametric
    ) where

import Lang.Numbers
import Exec.Region

-- | A dynamical system of the form $f : \mathbb{C} \rightarrow \mathbb{C}$.
data ComplexDynamics = ComplexDynamics
    { function :: C -> C              -- ^ The function to iterate.
    , continue :: C -> Int -> Bool    -- ^ Check if the iteration should continue.
    , classify :: C -> Int -> Region  -- ^ Determine which region the iteration results belong to.
    }

-- | Convert a description of a dynamical system to a Haskell function.
runComplexDynamics :: ComplexDynamics -> Dynamics C
runComplexDynamics dyn = Dynamics $ \z -> classify' $ head $ dropWhile (uncurry $ continue dyn) (trace z)
    where trace :: C -> [(C,Int)]
          trace z = zip (iterate f z) [0..] 
          f = function dyn
          classify' (z, n) = Result ((classify dyn) z n) z n

data ParametricComplexDynamics = ParametricComplexDynamics
    { family   :: C -> C -> C          -- ^ The family of functions.
    , initialValue :: C -> C           -- ^ The initial z-value to trace for a given parameter value.
    , continueP :: C -> Int -> Bool    -- ^ Check if the iteration should continue.
    , classifyP :: C -> Int -> Region  -- ^ Determine which region the iteration results belong to.
    }


-- | Convert a description of a parameterized dynamical system to a Haskell function.
runParametric :: ParametricComplexDynamics -> Dynamics C
runParametric pdyn = Dynamics $ \c -> classify' $ head $ dropWhile (uncurry $ continueP pdyn) (trace c)
    where trace :: C -> [(C,Int)]
          trace c = zip (iterate (f c) (z0 c)) [0..] 
          f  = family pdyn
          z0 = initialValue pdyn
          classify' (z, n) = Result ((classifyP pdyn) z n) z n