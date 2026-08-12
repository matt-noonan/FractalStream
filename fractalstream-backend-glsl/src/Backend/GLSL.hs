-- | GLSL code generator for FractalStream viewer scripts.
--
-- The OpenGL app reuses fractalstream-core's parser, typechecker and typed
-- AST, and this package compiles the resulting 'Language.Code.Code' to GLSL
-- fragment-shader source.
--
-- Entry point: 'compileViewerScript' parses a FractalStream source string and
-- returns GLSL fragment-shader body code ready to be spliced into @main()@.
module Backend.GLSL
  ( module Backend.GLSL.Types
  , module Backend.GLSL.Prelude
  , module Backend.GLSL.Compile
  ) where

import Backend.GLSL.Types
import Backend.GLSL.Prelude
import Backend.GLSL.Compile
