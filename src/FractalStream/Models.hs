module FractalStream.Models
    ( Coordinate
    , ComplexParametric1d
    ) where

import Lang.Numbers

type family Coordinate model :: *
data ComplexParametric1d


{-
import Data.Word
import Data.Complex


  
newtype ViewCoordinate a = ViewCoordinate a

data ParameterType = N | Z | R | C

data Parameter = Parameter
  { parameterType :: !ParameterType
  , parameterName :: !String
  , parameterDesc :: !String
  }

data Group = Group
  { groupTitle  :: !String
  , groupParams :: [Parameter]
  }
  
epsilon :: Parameter
epsilon = Parameter
  { parameterType = R
  , parameterName = "Epsilon"
  , parameterDesc = concat
      [ "How close together two complex numbers should"
      , " be in order for them to be considered equal?" ]
  }

infinity :: Parameter
infinity = Parameter
  { parameterType = R
  , parameterName = "Infinity"
  , parameterDesc = concat
      [ "How large a number should be in order to"
      , " consider it escaped / near infinity?" ]
  }

maxIter :: Parameter
maxIter = Parameter
  { parameterType = N
  , parameterName = "Maximum iteration count"
  , parameterDesc = concat
      [ "How many iterations should we try before giving up?" ]
  }

data Int32

data ParameterDesc where
  = Param Parameter
  | 
  
complexParametric1d
  =  viewCoordinate @(Complex Double) "C"
  <> parameter @Int32 "Maximum iteration count"
  <> parameter @Double "Epsilon"
  <> parameter @Double "Infinity"

complexParametricDynamics1d
  =  viewCoordinate @(Complex Double) "Z"
  <> parameter @(Complex Double) "C"
  <> parameter @Int32 "Maximum iteration count"
  <> parameter @Double "Epsilon"
  <> parameter @Double "Infinity"

complexDynamics1d
  =  viewCoordinate @(Complex Double) "Z"
  <> parameter @Int32 "Maximum iteration count"
  <> parameter @Double "Epsilon"
  <> parameter @Double "Infinity"
-}

type instance Coordinate ComplexParametric1d = C --(Double, Double)
