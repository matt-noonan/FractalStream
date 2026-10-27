-- | The GLSL uses the following auxiliary code:
--    - Color constants
--    - Complex-arithmetic function (@_cMul@, @_cExp@, …) for the plane view
--    - Projective helpers (@_pAdd@, @_pMul@, …) for the Riemann-sphere view.
module Backend.GLSL.Prelude
  ( fragConstants
  , complexHelpers
  , projectiveHelpers
  , runtimePrelude
  ) where

-- | @#define@s for the named colour constants and a couple of math constants.
fragConstants :: String
fragConstants = unlines
  [ "#define M_PI 3.1415926535897932384626433832795"
  , "#define EULER 2.718281828459045235360287471352"
  , "#define WHITE (vec4(1.0, 1.0, 1.0, 1.0))"
  , "#define BLACK (vec4(0.0, 0.0, 0.0, 1.0))"
  , "#define RED (vec4(1.0, 0.0, 0.0, 1.0))"
  , "#define GREEN (vec4(0.0, 1.0, 0.0, 1.0))"
  , "#define BLUE (vec4(0.0, 0.0, 1.0, 1.0))"
  , "#define YELLOW (vec4(1.0, 1.0, 0.0, 1.0))"
  , "#define MAGENTA (vec4(1.0, 0.0, 1.0, 1.0))"
  , "#define CYAN (vec4(0.0, 1.0, 1.0, 1.0))"
  ]

-- | Complex arithmetic on @vec2@ (plane view).
complexHelpers :: String
complexHelpers = unlines
  [ "vec2 _cMul(vec2 a, vec2 b) {"
  , "  return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);"
  , "}"
  , ""
  , "vec2 _cDiv(vec2 a, vec2 b) {"
  , "  return vec2(a.x * b.x + a.y * b.y, a.y * b.x - a.x * b.y) / (b.x * b.x + b.y * b.y);"
  , "}"
  , ""
  , "vec2 _cSqrt(vec2 a) {"
  , "  float sqrtR = sqrt(length(a));"
  , "  float theta = atan(a.y, a.x);"
  , "  return vec2(sqrtR * cos(theta / 2.0), sqrtR * sin(theta / 2.0));"
  , "}"
  , ""
  , "vec2 _cPow(vec2 a, float b) {"
  , "  float powR = pow(length(a), b);"
  , "  float theta = atan(a.y, a.x);"
  , "  return vec2(powR * cos(b * theta), powR * sin(b * theta));"
  , "}"
  , ""
  , "vec2 _cExp(vec2 a) {"
  , "  float expR = exp(a.x);"
  , "  return vec2(expR * cos(a.y), expR * sin(a.y));"
  , "}"
  , ""
  , "vec2 _cLog(vec2 a) {"
  , "  return vec2(log(length(a)), atan(a.y, a.x));"
  , "}"
  ]

-- | Complex arithmetic on @vec4@ projective coordinates (Riemann-sphere view).
-- A point is @[z0 : z1]@ stored as @(re z0, im z0, re z1, im z1)@; @infinity@ is
-- @vec4(1,0,0,0)@.
projectiveHelpers :: String
projectiveHelpers = unlines
  [ "vec4 _pAdd(vec4 a, vec4 b) {"
  , "  return normalize(vec4("
  , "    a.x * b.z - a.y * b.w + a.z * b.x - a.w * b.y,"
  , "    a.x * b.w + a.y * b.z + a.z * b.y + a.w * b.x,"
  , "    a.z * b.z - a.w * b.w,"
  , "    a.z * b.w + a.w * b.z"
  , "  ));"
  , "}"
  , ""
  , "vec4 _pSub(vec4 a, vec4 b) {"
  , "  return normalize(vec4("
  , "    a.x * b.z - a.y * b.w - a.z * b.x + a.w * b.y,"
  , "    a.x * b.w + a.y * b.z - a.z * b.y - a.w * b.x,"
  , "    a.z * b.z - a.w * b.w,"
  , "    a.z * b.w + a.w * b.z"
  , "  ));"
  , "}"
  , ""
  , "vec4 _pOpp(vec4 a) { return vec4(-a.xy, a.zw); }"
  , ""
  , "vec4 _pMul(vec4 a, vec4 b) {"
  , "  return normalize(vec4("
  , "    a.x * b.x - a.y * b.y,"
  , "    a.x * b.y + a.y * b.x,"
  , "    a.z * b.z - a.w * b.w,"
  , "    a.z * b.w + a.w * b.z"
  , "  ));"
  , "}"
  , ""
  , "vec4 _pDiv(vec4 a, vec4 b) {"
  , "  return normalize(vec4("
  , "    a.x * b.z - a.y * b.w,"
  , "    a.x * b.w + a.y * b.z,"
  , "    a.z * b.x - a.w * b.y,"
  , "    a.z * b.y + a.w * b.x"
  , "  ));"
  , "}"
  , ""
  , "vec4 _pInv(vec4 a) { return vec4(a.zw, a.xy); }"
  , ""
  , "float _pDist(vec4 a, vec4 b) {"
  , "  return length(vec2("
  , "    a.x * b.z - a.y * b.w - a.z * b.x + a.w * b.y,"
  , "    a.x * b.w + a.y * b.z + a.z * b.y + a.w * b.x"
  , "  ));"
  , "}"
  ]

-- | The whole prelude (constants + both helper sets), ready to splice ahead of
-- generated @main@ code.
runtimePrelude :: String
runtimePrelude =
  unlines [ fragConstants, complexHelpers, projectiveHelpers ]
