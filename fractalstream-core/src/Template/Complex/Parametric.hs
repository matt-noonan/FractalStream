module Template.Complex.Parametric
  ( StoppingCondition(..)
  , SimpleComplexParametric(..)
  , ComplexParametric(..)
  , Colorizer(..)
  ) where

import Language.Value
import Data.List.NonEmpty (NonEmpty)
--import qualified Data.List.NonEmpty as NE

data StoppingCondition env
  = StopWhenTrue     (Value env 'BooleanT)
  | StopOnAnyCycle   (Value env 'RealT) (Value env 'IntegerT)
  | StopOnExactCycle (Value env 'RealT) (Value env 'IntegerT)

data Colorizer env
  = Autocolor
  | Gradient (NonEmpty (Value env 'ColorT))

data SimpleComplexParametric env = SimpleComplexParametric
  { scpInitialZ :: Value env 'ComplexT
  , scpFunction :: Value env 'ComplexT
  , scpStopWhen :: StoppingCondition env
  , scpColorize :: NonEmpty (Value env 'BooleanT, Value env 'ColorT)
  }

data ComplexParametric = ComplexParametric
{-
scpToProject :: String
             -> String
             -> ProjectSource
  -}

{-
Template should have:

- a TITLE
- a PARAMETER ENVIRONMENT of immutable variables and types,
   with prescribed but editable initial conditions
- a FIXED ENVIRONMENT of immutable variables and types,
   which are not user-editable but may depend on the parameter
   environment. Each variable can be an expression of the previous ones.
- a USER-DEFINED ENVIRONMENT
- a series of LABELED VALUES or LABELED CODE, using the
   union of the parameter-, fixed-, and user-defined environments.
- a PROJECT which may reference the labeled values or code.

--[ Viewer refresh ]-------------------------
old_image = image
image : Image <- render dim corner delta
  C : Complex
  C = x + i y
  z = {{z0}} : Complex
  k = 0 : Natural
  repeat maxIter times
    z = {{function}}
    k = k + 1
  while not {{stop}}

  {{colorCases}}

animate = 0.0
---------------------------------------------

template z0 : Complex Code with
  C : Complex
template function : Complex with
  C : Complex, z : Complex
template stop : Complex with
  C : Complex, z : Complex
template colorCases : Color Code with
  steps : Natural, C : Complex, z : Complex

--[ Viewer timer ]-------------------------
old_image = image
image : Image <- render dim corner delta
  C : Complex
  C = x + i y
  z = {{z0}} : Complex
  k = 0 : Natural
  repeat maxIter times
    z = {{function}}
    k = k + 1
  while not {{stop}}

  {{colorCases}}

animate = 0.0
---------------------------------------------

  z0 : C
  z0 = {{z0}}
  z : C
  z = x + i y
  set stroke color white
  repeat 100 times
    z0 = z
    z  = {{function}}
    draw point at z
    draw line from z0 to z
-}
