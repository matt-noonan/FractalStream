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
  , stmtNames
  , definedName
  ) where

import qualified Data.Set as Set
import Data.Set (Set)

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

-- | Every variable and function name mentioned in a statement (an
-- over-approximation of what it depends on: scoping is ignored).
stmtNames :: Stmt -> Set String
stmtNames (Stmt _ node) = case node of
  SBlock body -> foldMap stmtNames body
  SDecl n _ e -> Set.insert n (exprNames e)
  SSet n e -> Set.insert n (exprNames e)
  SFun f _ e -> Set.insert f (exprNames e)
  SIf c yes no -> exprNames c <> stmtNames yes <> stmtNames no
  SLoop _ c lim body -> exprNames c <> foldMap exprNames lim <> stmtNames body
  SIterate n e _ c lim -> Set.insert n (exprNames e <> exprNames c <> foldMap exprNames lim)
  SPass -> Set.empty
  SDraw d -> case d of
    DrawPointAt p -> exprNames p
    DrawCircleAt _ c r -> exprNames c <> exprNames r
    DrawRectFrom _ a b -> exprNames a <> exprNames b
    DrawLineFrom a b -> exprNames a <> exprNames b
    UseStroke c -> exprNames c
    UseFill c -> exprNames c
    EraseAll -> Set.empty
    WriteAt t p -> exprNames t <> exprNames p
  SForEach item list body -> Set.fromList [item, list] <> stmtNames body
  SWithFirst item p list body fallback ->
    Set.fromList [item, list] <> exprNames p <> stmtNames body <> foldMap stmtNames fallback

-- | The name a declaration or function definition defines.
definedName :: Stmt -> Maybe String
definedName (Stmt _ node) = case node of
  SDecl n _ _ -> Just n
  SFun f _ _ -> Just f
  _ -> Nothing
