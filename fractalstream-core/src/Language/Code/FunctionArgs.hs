-- | Functions given as arguments: to a function (of either kind) or to a
-- code block, a function can be given by its name (@twice(sq, 3)@) or
-- written where it's given (@twice(w -> w + 1, 3)@).
--
-- Whether a parameter is a function is decided at each call, by what it's
-- given: so a parameter applied to something in the body (@g(x)@, or
-- @g x@, which parse as products) is a call where the argument is a
-- function, and a product where it's a number. A function parameter is
-- only ever applied, or given on to another call: anything else is an
-- error, when the result is typed.
--
-- Since everything is expanded inline, this costs nothing when the code
-- runs: a call @g(x)@ with @g@ given as @sq@ becomes @sq(x)@, and with @g@
-- given as @w -> w + 1@, a local binding of a fresh @w@ to @x@ around
-- @w + 1@.
module Language.Code.FunctionArgs
  ( isFunctionArg
  , substituteFunction
  , substituteFunctionInStmt
  ) where

import Language.Typecheck
import Language.Value.Syntax
import Language.Code.Syntax
import Language.Code.Previous (mapStmtExprs)
import Language.Code.Typecheck.Functions (isFunctionArg, substituteFunction)

-- | Put the function @fn@ in place of the parameter @p@, in a statement.
-- The tag makes the names it binds (for a function written in place)
-- different from those of any other expansion.
substituteFunctionInStmt :: String -> String -> Expr -> Stmt -> TC Stmt
substituteFunctionInStmt tag p fn = mapStmtExprs (substituteFunction tag p fn)

