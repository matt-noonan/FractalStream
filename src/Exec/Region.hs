module Exec.Region where

data Region = Interior | Exterior deriving (Eq, Ord, Show)

data Result a = Result Region a Int

newtype Dynamics a = Dynamics (a -> Result a)

runDynamics :: Dynamics a -> a -> Result a
runDynamics (Dynamics dyn) = dyn

