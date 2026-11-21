-- | Tables of builtin names (functions, colors, types) and the names of
-- the internal bookkeeping variables, shared by the parser, the typing
-- pass, and the elaborator.
module Language.Value.Builtins where

import FractalStream.Prelude

import qualified Data.Map as Map

import Language.Value
import Data.Color

internalEscapeRadius, internalVanishingRadius, internalIterations :: String
internalStuck, internalIterationLimit :: String
internalEscapeRadius      = "[internal] escape radius"
type InternalEscapeRadius = "[internal] escape radius"
internalVanishingRadius      = "[internal] vanishing radius"
type InternalVanishingRadius = "[internal] vanishing radius"
internalIterations      = "[internal] iteration count"
type InternalIterations = "[internal] iteration count"
internalIterationLimit      = "[internal] iteration limit"
type InternalIterationLimit = "[internal] iteration limit"
type InternalStuck = "[internal] stuck"

-- | The name under which a sphere viewer's code, and its tools', are told
-- that they're on the sphere (a truth value no script can name): their
-- points are points of Ĉ, which their drawings are drawn at as they are.
sphereViewMarker :: String
sphereViewMarker = "[internal] sphere view"
internalStuck      = "[internal] stuck"

-- | The names under which a tool is told how its viewer shows its points
-- (see "Actor.Ensemble"): the kind of view (0 the plane, 1 the sphere, 2
-- the hyperbolic plane), the width of a pixel on the screen plane, and in
-- the hyperbolic plane, part of the map from the screen to the model (see
-- 'viewMobiusNames').
viewKindName, viewPixelSizeName :: String
viewKindName = "[internal] view kind"
viewPixelSizeName = "[internal] view pixel size"

-- (the real and imaginary parts of @a@ and @c@ of the Möbius map, of
-- determinant 1, from the screen to the model: see
-- 'Actor.Viewer.Disk.viewMobius')
viewMobiusNames :: [String]
viewMobiusNames = [ "[internal] view " ++ n | n <- ["a.re", "a.im", "c.re", "c.im"] ]

type Fun a b = forall env value. KnownEnvironment env => Eval (value '(env, a)) -> ValueF value '(env, b)

data CommonFun = CommonFun (Fun 'RealT 'RealT) (Fun 'ComplexT 'ComplexT)

colors :: Map String Color
colors = Map.fromList
  [ ("red", red), ("green", green), ("blue", blue)
  , ("black", black), ("white", white), ("grey", grey), ("gray", grey)
  , ("orange", orange), ("yellow", yellow), ("purple", purple), ("violet", violet)
  , ("pink", pink)]

realFunctions :: Map String RealFun
realFunctions = Map.fromList
  [ ("arccos",  RealFun ArccosF)
  , ("arcsin",  RealFun ArcsinF)
  , ("arctan",  RealFun ArctanF)
  , ("arccosh", RealFun ArccoshF)
  , ("arcsinh", RealFun ArcsinhF)
  , ("arctanh", RealFun ArctanhF)
  ]

commonFunctions :: Map String CommonFun
commonFunctions = Map.fromList
  [ ("exp",  CommonFun ExpF   ExpC)
  , ("log",  CommonFun LogF   LogC)
--  , ("abs", CommonFun AbsF AbsC)
  , ("sqrt", CommonFun SqrtF  SqrtC)
  , ("cos",  CommonFun CosF   CosC)
  , ("sin",  CommonFun SinF   SinC)
  , ("tan",  CommonFun TanF   TanC)
  , ("cosh", CommonFun CoshF  CoshC)
  , ("sinh", CommonFun SinhF  SinhC)
  , ("tanh", CommonFun TanhF  TanhC)
  ]

data RealFun = RealFun (Fun 'RealT 'RealT)
data RoundingFun = RoundingFun (Fun 'RealT 'IntegerT)
data ComplexFun where
  ComplexFun :: forall ty. TypeProxy ty -> Fun 'ComplexT ty -> ComplexFun

complexFunctions :: Map String ComplexFun
complexFunctions = Map.fromList
  [ ("re",  ComplexFun RealType ReC)
  , ("im",  ComplexFun RealType ImC)
  , ("arg", ComplexFun RealType ArgC)
  , ("conj", ComplexFun ComplexType ConjC)
  ]

roundingFunctions :: Map String RoundingFun
roundingFunctions = Map.fromList
  [ ("round",   RoundingFun RoundF)
  , ("floor",   RoundingFun FloorF)
  , ("ceiling", RoundingFun CeilingF)
  ]

types :: Map String FSType
types = Map.fromList
  [ ("R", RealT), ("ℝ", RealT), ("Real", RealT)
  , ("Z", IntegerT), ("ℤ", IntegerT), ("Int", IntegerT), ("Integer", IntegerT)
  , ("C", ComplexT), ("ℂ", ComplexT), ("Complex", ComplexT)
  , ("Bool", BooleanT), ("Boolean", BooleanT)
  , ("Color", ColorT), ("Text", TextT)
  -- (ℙ¹ is also `ℙ¹` and `P^1`: see 'Language.Value.Parser.typeGrammar')
  , ("ℙ", ProjectiveT), ("P1", ProjectiveT)
  , ("Ĉ", ExtComplexT), ("ℂ\x0302", ExtComplexT), ("Cinf", ExtComplexT)
  ]
