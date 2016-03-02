{- |
Module      : Exec.Region
Description : Definitions for regions of parameter- and dynamical-space, and iteration results.
-}
module Exec.Region where

-- | A region in the parameter- or dynamical-plane.
data Region = Interior | Exterior deriving (Eq, Ord, Show)
--type Region = Int

-- | The result of an iteration. The type parameter determines the type of the
--   dynamics: real, complex, real-planar, etc.
data Result a
    = Result Region a Int -- ^ A tuple of region, final value of the trace, and number of iterations taken.

-- | Dynamics encapsulates a complete iteration which begins with an initial value and produces a result.
newtype Dynamics a = Dynamics (a -> Result a)

-- | Apply encapsulated dynamics to an initial value.
runDynamics :: Dynamics a -> a -> Result a
runDynamics (Dynamics dyn) = dyn

