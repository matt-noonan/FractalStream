-- | Typechecking for value expressions: the entry point that takes a
-- parsed surface expression to a typed 'Value' at a requested type, by
-- running the typing pass ("Language.Value.Typing") in the current
-- environment and then elaborating ("Language.Value.Elaborate").
module Language.Value.Typecheck
  ( ParsedValue
  , atType
  , module Language.Value.Builtins
  ) where

import FractalStream.Prelude

import Language.Value
import Language.Value.Syntax
import Language.Value.Builtins
import Language.Value.Typing
import Language.Value.Elaborate
import Language.Typecheck

-- | Parsed values, which have not yet been checked for
-- type / scope correctness.
type ParsedValue = Expr

-- | Interpret the value at the given type, inserting conversions if needed.
atType :: forall env ty
        . (KnownEnvironment env, KnownType ty)
       => ParsedValue
       -> TypeProxy ty
       -> TC (Value '(env, ty))
atType v ty = do
  tv <- check (typeEnvOf (envProxy (Proxy @env))) (typeToFS ty) v
  elaborate ty tv
