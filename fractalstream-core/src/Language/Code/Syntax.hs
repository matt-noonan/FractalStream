-- | The surface (untyped) syntax of code, as produced by the parser.
-- Blocks are flat lists of statements: the scope of a variable declaration
-- (or function definition) is the rest of its block, which is worked out
-- during elaboration ("Language.Code.Typecheck") rather than by the
-- grammar.
module Language.Code.Syntax
  ( Stmt(..)
  , StmtF(..)
  , DrawStmt(..)
  , LoopKind(..)
  ) where

import Language.Type
import Language.Parser.SourceRange
import Language.Value.Syntax

data Stmt = Stmt { stmtRange :: SourceRange, stmtNode :: StmtF }
  deriving Show

data LoopKind
  = LoopWhile     -- ^ @while c: body@
  | LoopUntil     -- ^ @until c: body@
  | LoopDoWhile   -- ^ @repeat: body while c@
  | LoopDoUntil   -- ^ @repeat: body until c@
  deriving (Eq, Show)

data StmtF
  = SBlock [Stmt]
    -- ^ A block of statements, with its own scope
  | SDecl String (Maybe FSType) Expr
    -- ^ @x : T <- e@, or @x := e@ (with the type taken from @e@)
  | SSet String Expr
  | SFun String [String] Expr
    -- ^ @let f(x, y) = e@
  | SIf Expr Stmt Stmt
  | SLoop LoopKind Expr (Maybe Expr) Stmt
    -- ^ loop kind, condition, optional iteration limit, body
  | SIterate String Expr Bool Expr (Maybe Expr)
    -- ^ @iterate x -> e while/until c up to n times@
  | SPass
  | SDraw DrawStmt
  | SForEach String String Stmt
    -- ^ @for each item in list: body@
  | SWithFirst String Expr String Stmt (Maybe Stmt)
    -- ^ @with first item matching p in list: body else: fallback@
  deriving Show

data DrawStmt
  = DrawPointAt Expr
  | DrawCircleAt Bool Expr Expr
  | DrawRectFrom Bool Expr Expr
  | DrawLineFrom Expr Expr
  | UseStroke Expr
  | UseFill Expr
  | EraseAll
  | WriteAt Expr Expr
  deriving Show
