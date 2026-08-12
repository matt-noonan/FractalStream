-- | Code statement → GLSL codegen.
--
-- 'codeToGlsl' walks a typed 'Code' AST and emits GLSL statements into the
-- shared 'GlslM' writer stream.  Value sub-expressions are compiled via
-- 'valueToGlsl'. Declarations are emitted before the enclosing statement.
module Backend.GLSL.Code
  ( codeToGlsl
  ) where

import Language.Code
import Backend.GLSL.Types
import Backend.GLSL.Value

import Control.Monad.Writer.Strict (tell)
import GHC.TypeLits                (symbolVal)

-- | Compile a 'Code' block to a sequence of GLSL statements.
-- The statements (and any needed declarations) are appended to the
-- 'GlslM' writer stream.
codeToGlsl :: ComplexRep -> Code env -> GlslM ()
codeToGlsl cr = go
  where
    go :: forall env'. Code env' -> GlslM ()
    go = \case
      Let _ name val body -> do
        valExpr <- valueToGlsl cr val
        let varName = sanitizeVar (symbolVal name)
            ty      = either (\e -> error ("GLSL type error in Let: " ++ e)) id
                        (glslType cr (typeOfValue val))
        tell [ty ++ " " ++ varName ++ " = " ++ valExpr ++ ";"]
        go body

      Set _ name val -> do
        valExpr <- valueToGlsl cr val
        tell [sanitizeVar (symbolVal name) ++ " = " ++ valExpr ++ ";"]

      Block stmts -> mapM_ go stmts

      NoOp -> pure ()

      DoWhile cond body -> do
        tell ["do {"]
        go body
        condExpr <- valueToGlsl cr cond
        tell ["} while (" ++ condExpr ++ ");"]

      IfThenElse cond yes no -> do
        condExpr <- valueToGlsl cr cond
        tell ["if (" ++ condExpr ++ ") {"]
        go yes
        tell ["} else {"]
        go no
        tell ["}"]

      -- Draw is not implemented in shaders body
      DrawCommand _ -> pure ()

      -- List operations not supported
      Lookup {}  -> pure ()
      ForEach {} -> pure ()
