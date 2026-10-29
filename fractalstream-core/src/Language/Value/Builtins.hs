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
internalStuck      = "[internal] stuck"

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
  , ("acos",    RealFun ArccosF)
  , ("asin",    RealFun ArcsinF)
  , ("atan",    RealFun ArctanF)
  , ("arccosh", RealFun ArccoshF)
  , ("arcsinh", RealFun ArcsinhF)
  , ("arctanh", RealFun ArctanhF)
  , ("acosh",   RealFun ArccoshF)
  , ("asinh",   RealFun ArcsinhF)
  , ("atanh",   RealFun ArctanhF)
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
  , ("Re",  ComplexFun RealType ReC)
  , ("im",  ComplexFun RealType ImC)
  , ("Im",  ComplexFun RealType ImC)
  , ("arg", ComplexFun RealType ArgC)
  , ("Arg", ComplexFun RealType ArgC)
  , ("conj", ComplexFun ComplexType ConjC)
  , ("Conj", ComplexFun ComplexType ConjC)
  , ("bar", ComplexFun ComplexType ConjC)
  , ("Bar", ComplexFun ComplexType ConjC)
  ]

roundingFunctions :: Map String RoundingFun
roundingFunctions = Map.fromList
  [ ("round",   RoundingFun RoundF)
  , ("floor",   RoundingFun FloorF)
  , ("ceiling", RoundingFun CeilingF)
  , ("ceil",    RoundingFun CeilingF)
  ]

types :: Map String FSType
types = Map.fromList
  [ ("R", RealT), ("ℝ", RealT), ("Real", RealT)
  , ("Z", IntegerT), ("ℤ", IntegerT), ("Int", IntegerT), ("Integer", IntegerT)
  , ("C", ComplexT), ("ℂ", ComplexT), ("Complex", ComplexT)
  , ("Bool", BooleanT), ("Boolean", BooleanT)
  , ("Color", ColorT), ("Text", TextT)
  ]

-- | Color wheels: each cycles through 8 colors over the [0,1] interval.
wheels :: Map String [Color]
wheels = Map.fromList
  [ ("rainbow", [red, orange, yellow, green, rgbToColor (0, 128, 128), blue, purple, violet])
  -- Sampled from 8 points in the Arenberg phase wheel
  -- (https://eprovst.github.io/DomainColoring.jl/stable/arenberg/#The-Arenberg-Phase-Wheel)
  -- and converted to sRGB using D50 at 2 degrees
  , ("arenberg", map rgbToColor
      [ (255,  80,  81)
      , (255, 174,   0)
      , (152, 190,   0)
      , (  0, 183,  74)
      , (  0, 237, 232)
      , (  0, 172, 255)
      , (184, 161, 255)
      , (255, 134, 226) ])
  -- Color maps from https://s-ink.org/scientific-colour-maps
  , ("romao", map rgbToColor
      [ (201, 218, 169)
      , (196, 163,  72)
      , (164, 103,  43)
      , (137,  64,  53)
      , (116,  57,  88)
      , ( 93,  84, 140)
      , ( 81, 135, 185)
      , (136, 197, 201) ])
  , ("bamo", map rgbToColor
      [ (208, 200, 189)
      , (212, 162, 196)
      , (178, 110, 160)
      , (118,  59, 102)
      , ( 78, 45, 63)
      , ( 79,  69,  59)
      , (105, 110,  75)
      , (177, 186, 150) ])
  , ("broco", map rgbToColor
      [ (201, 203, 179)
      , (139, 162, 190)
      , ( 89, 115, 155)
      , ( 57,  65, 100)
      , ( 57,  46,  55)
      , ( 69,  63,  41)
      , (109, 105,  60)
      , (166, 165, 118) ])
  , ("corko", map rgbToColor
      [ (165, 196, 173)
      , (134, 157, 185)
      , ( 86, 111, 148)
      , ( 64,  68,  93)
      , ( 64,  62,  57)
      , ( 69,  79,  43)
      , ( 79, 115,  58)
      , (117, 164, 111) ])
  , ("viko", map rgbToColor
      [ (210, 179, 161)
      , (119, 157, 187)
      , ( 61, 104, 151)
      , ( 60,  53, 103)
      , ( 80,  25,  59)
      , (105,  24,  34)
      , (150,  62,  35)
      , (203, 135,  99) ])
  -- Sawtooth gradients
  , ("ice", sawtooth
      [ (0x1e, 0xcb, 0xff)
      , (0x5f, 0xd8, 0xfb)
      , (0x89, 0xe3, 0xf9)
      , (0xad, 0xef, 0xf9)
      , (0xcf, 0xf9, 0xfc) ])
  , ("fire", sawtooth
      [ (0xff, 0x3d, 0x3d)
      , (0xff, 0x7b, 0x39)
      , (0xff, 0xab, 0x47)
      , (0xff, 0xd4, 0x67)
      , (0xff, 0xfa, 0x94) ])
  , ("wheat", sawtooth
      [ (0x00, 0xb3, 0xff)
      , (0x66, 0xaf, 0xe5)
      , (0x8a, 0xac, 0xcb)
      , (0xf3, 0xd7, 0xa4)
      , (0xed, 0xf2, 0x00) ])
  , ("rose", sawtooth
      [ (0xff, 0x00, 0x99)
      , (0xff, 0x58, 0x62)
      , (0xf7, 0x92, 0x41)
      , (0xd5, 0xbf, 0x4d)
      , (0xb3, 0xe2, 0x83) ])
  , ("forest", sawtooth
      [ (0x0c, 0x49, 0x00)
      , (0x42, 0x57, 0x12)
      , (0x67, 0x64, 0x2c)
      , (0x86, 0x72, 0x4a)
      , (0x9f, 0x83, 0x6b) ])
  , ("ocean", sawtooth
      [ (0x0a, 0x00, 0x49)
      , (0x00, 0x34, 0x6a)
      , (0x04, 0x5e, 0x7d)
      , (0x54, 0x87, 0x8c)
      , (0x98, 0xad, 0xa7) ])
  , ("winter", sawtooth
      [ (0xf7, 0xfa, 0xff)
      , (0xc3, 0xcb, 0xdf)
      , (0x94, 0x9d, 0xc0)
      , (0x6b, 0x71, 0xa0)
      , (0x46, 0x46, 0x80) ])
  , ("spring", sawtooth
      [ (0x67, 0xdf, 0x82)
      , (0x88, 0xcf, 0x57)
      , (0xa4, 0xbc, 0x31)
      , (0xbd, 0xa7, 0x0e)
      , (0xd3, 0x8e, 0x08) ])
  , ("summer", sawtooth
      [ (0x30, 0x5c, 0xce)
      , (0x00, 0x87, 0xb4)
      , (0x00, 0xa1, 0xb1)
      , (0x00, 0xc1, 0x95)
      , (0x23, 0xe3, 0x00) ])
  , ("fall", sawtooth
      [ (0x80, 0x37, 0x03)
      , (0x9e, 0x63, 0x1a)
      , (0xba, 0x8f, 0x37)
      , (0xd4, 0xbd, 0x5a)
      , (0xeb, 0xec, 0x83) ])
  ]
  where
    -- A gradient there and back again: c1 .. c5 .. c2
    sawtooth cs = let cs' = map rgbToColor cs in cs' ++ reverse (drop 1 (take 4 cs'))
