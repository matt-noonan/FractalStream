-- | Top-level entry point: parse a FractalStream viewer script and compile it
-- to GLSL fragment-shader main-body source.
--
-- Usage:
-- @
--   case compileViewerScript isProjective coordName fsSource of
--     Left err   -> … -- parse / typecheck error
--     Right glsl -> … -- GLSL statements to splice into main()
-- @
module Backend.GLSL.Compile
  ( compileViewerScript
  ) where

import Language.Code         (transformValues)
import Language.Code.Parser  (parseCode, noSplices)
import Language.Environment  (withEnvFromMap)
import Language.Type         (SomeType(..), TypeProxy(..))
import Language.Value.Transform (integerPowers, avoidSqrt)
import Backend.GLSL.Types    (vec2Rep, projectiveRep)
import Backend.GLSL.Value    (runGlslM)
import Backend.GLSL.Code     (codeToGlsl)

import qualified Data.Map.Strict as Map

-- ---------------------------------------------------------------------------
-- Environment construction
-- ---------------------------------------------------------------------------

-- | Build the Map that represents the viewer's full pre-declared environment.
--
-- This mirrors 'InternalViewerEnv' from @Actor.Viewer.Types@ (in
-- fractalstream-core) but as a runtime map rather than a type-level list.
-- The coord variable name (e.g. @\"c\"@) is added as a 'ComplexT' binding.

viewerEnvMap :: String -> Map.Map String SomeType
viewerEnvMap coordName = Map.fromList
  [ (coordName,                         SomeType ComplexType)
  , ("color",                           SomeType ColorType)
  , ("[internal] dx",                   SomeType RealType)
  , ("[internal] dy",                   SomeType RealType)
  , ("[internal] escape radius",        SomeType RealType)
  , ("[internal] iteration count",      SomeType IntegerType)
  , ("[internal] iteration limit",      SomeType IntegerType)
  , ("[internal] stuck",                SomeType BooleanType)
  , ("[internal] vanishing radius",     SomeType RealType)
  , ("[internal] x",                    SomeType RealType)
  , ("[internal] y",                    SomeType RealType)
  ]

-- ---------------------------------------------------------------------------
-- Main compiler
-- ---------------------------------------------------------------------------

-- | Parse and compile a FractalStream viewer script to GLSL.
--
-- Returns either an error message or the GLSL code to splice inside
-- @void main() { … }@ (after the coord variable is initialised and before
-- the closing brace).
--
-- The generated code assumes the following are already in scope:
--   * the coord variable (e.g. @vec2 c = …@) — provided by the viewer
--   * standard uniforms: @_max_iterations@, @_escape_radius@,
--     @_convergence_radius@ — provided by the shader header
--   * @out vec4 color@ — provided by the shader header
--
-- Two local variables are declared in the generated preamble:
--   * @int _iter_count = 0@ — mutable iteration-count tracker
--   * @bool _stuck = false@ — mutable stuck flag
compileViewerScript
  :: Bool    -- ^ @True@ for the projective (Riemann sphere) view
  -> String  -- ^ name of the coordinate variable in the script (e.g. @\"c\"@)
  -> String  -- ^ FractalStream source code
  -> Either String String
compileViewerScript isProjective coordName source =
  let cr  = if isProjective then projectiveRep else vec2Rep
      env = viewerEnvMap coordName
  in withEnvFromMap env $ \envProxy ->
       case parseCode envProxy noSplices source of
         Left (Left parseErr) -> Left (show parseErr)
         Left (Right tcErr)   -> Left (show tcErr)
         Right code ->
           let code' = transformValues (integerPowers . avoidSqrt) code
               ((), stmts) = runGlslM (codeToGlsl cr code')
               preamble =
                 [ "int _iter_count = 0;"
                 , "bool _stuck = false;"
                 ]
           in Right (unlines (preamble ++ stmts))
