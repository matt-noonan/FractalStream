-- | Type mapping and the complex-representation abstraction for GLSL codegen.
--
-- The same FractalStream source can target two views that differ only in how
-- complex numbers are represented: the plane view uses @vec2@ with the @_c*@
-- helpers, the Riemann-sphere view uses @vec4@ projective coordinates with the
-- @_p*@ helpers.  Parametrising the code generator over a 'ComplexRep' lets one
-- AST compile to either, instead of hand-writing each dynamical system twice.
module Backend.GLSL.Types
  ( ComplexRep(..)
  , vec2Rep
  , projectiveRep
  , glslType
  , showF
  ) where

import Language.Type (TypeProxy(..), showType)

-- | How complex numbers are represented and operated on in generated GLSL.
-- Each field takes already-rendered GLSL expression strings and returns a new
-- expression string.
data ComplexRep = ComplexRep
  { crTypeName  :: String                          -- ^ GLSL type for @ComplexT@
  , crAdd       :: String -> String -> String
  , crSub       :: String -> String -> String
  , crMul       :: String -> String -> String
  , crDiv       :: String -> String -> String
  , crNeg       :: String -> String
  , crLit       :: Double -> Double -> String       -- ^ a complex literal (re, im)
  , crFromReal  :: String -> String                 -- ^ embed a real expr as complex
  , crConj      :: String -> String                 -- ^ complex conjugate
  }

-- | Plane view: @vec2@ with component-wise +/- and the @_c*@ helpers.
vec2Rep :: ComplexRep
vec2Rep = ComplexRep
  { crTypeName  = "vec2"
  , crAdd       = binop "+"
  , crSub       = binop "-"
  , crMul       = call2 "_cMul"
  , crDiv       = call2 "_cDiv"
  , crNeg       = \a -> "(-" ++ a ++ ")"
  , crLit       = \re im -> "vec2(" ++ showF re ++ ", " ++ showF im ++ ")"
  , crFromReal  = \a -> "vec2(" ++ a ++ ", 0.0)"
  , crConj      = \a -> "vec2(" ++ a ++ ".x, -" ++ a ++ ".y)"
  }

-- | Riemann-sphere view: @vec4@ projective coordinates with the @_p*@ helpers.
projectiveRep :: ComplexRep
projectiveRep = ComplexRep
  { crTypeName  = "vec4"
  , crAdd       = call2 "_pAdd"
  , crSub       = call2 "_pSub"
  , crMul       = call2 "_pMul"
  , crDiv       = call2 "_pDiv"
  , crNeg       = \a -> "_pOpp(" ++ a ++ ")"
  , crLit       = \re im -> "vec4(" ++ showF re ++ ", " ++ showF im ++ ", 1.0, 0.0)"
  , crFromReal  = \a -> "vec4(" ++ a ++ ", 0.0, 1.0, 0.0)"
  , crConj      = \a -> "vec4(" ++ a ++ ".x, -" ++ a ++ ".y, " ++ a ++ ".z, -" ++ a ++ ".w)"
  }

-- | GLSL type name for an FSType, given the complex representation.  Returns a
-- description on the left for types the GLSL backend cannot represent.
glslType :: ComplexRep -> TypeProxy t -> Either String String
glslType cr = \case
  BooleanType -> Right "bool"
  IntegerType -> Right "int"
  RealType    -> Right "float"
  ComplexType -> Right (crTypeName cr)
  ColorType   -> Right "vec4"
  ListType{}  -> Left "GLSL backend: lists are unsupported (no dynamic allocation in GLSL)"
  TextType    -> Left "GLSL backend: text is unsupported"
  ty          -> Left ("GLSL backend: unsupported type " ++ showType ty)

binop :: String -> String -> String -> String
binop op a b = "(" ++ a ++ " " ++ op ++ " " ++ b ++ ")"

call2 :: String -> String -> String -> String
call2 f a b = f ++ "(" ++ a ++ ", " ++ b ++ ")"

-- | Render a Double as a GLSL float literal (always with a decimal point).
showF :: Double -> String
showF x = let s = show x in if any (`elem` ".eE") s then s else s ++ ".0"
