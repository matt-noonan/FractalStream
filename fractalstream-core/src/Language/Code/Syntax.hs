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
  , Pattern(..)
  , patternNames
  , patternRange
  , stmtNames
  , definedNames
  ) where

import qualified Data.Set as Set
import Data.Set (Set)

import Language.Type
import Language.Parser.SourceRange
import Language.Value.Syntax

data Stmt = Stmt { stmtRange :: SourceRange, stmtNode :: StmtF }
  deriving Show

-- | What a tuple assignment assigns to: a variable, or a tuple of
-- patterns. Tuples nest to the right, as pairs do: @(a, b, c)@ is
-- @(a, (b, c))@.
data Pattern
  = PVar SourceRange String
  | PTuple SourceRange Pattern Pattern
  deriving Show

-- | The variables a pattern names, from left to right.
patternNames :: Pattern -> [String]
patternNames = \case
  PVar _ n -> [n]
  PTuple _ p q -> patternNames p ++ patternNames q

patternRange :: Pattern -> SourceRange
patternRange = \case
  PVar sr _ -> sr
  PTuple sr _ _ -> sr

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
    -- ^ @x : T := e@, or @x := e@ (with the type taken from @e@)
  | SSet String Expr
  | SDeclTuple Pattern Expr
    -- ^ @x, y := e@ (or @(x, y) := e@): declare each variable, with the
    -- matching part of @e@'s value (see "Language.Code.Tuples")
  | SSetTuple Pattern Expr
    -- ^ @x, y <- e@ (or @(x, y) <- e@): set each variable at once to the
    -- matching part of @e@'s value
  | SFun String [String] Expr
    -- ^ @f(x, y) := e@
  | SIf Expr Stmt Stmt
  | SLoop LoopKind Expr (Maybe Expr) Stmt
    -- ^ loop kind, condition, optional iteration limit, body
  | SIterate Pattern Expr Bool Expr (Maybe Expr)
    -- ^ @iterate x -> e while/until c up to n times@, or with a tuple:
    -- @iterate (x, y) -> (e1, e2) ...@
  | SPass
  | SDraw DrawStmt
  | SSetTable String Expr Expr
    -- ^ @T \@ (j, k) <- v@, or @T \@ point z <- v@, or a slice: @T \@ (j, *)
    -- <- xs@ (the index can have 'EPointIndex' and 'ESliceAll' parts)
  | SResize String Expr (Maybe Expr)
    -- ^ @resize T to (w, h)@, maybe @filled with v@
  | SAllocate String Expr (Maybe (Expr, Expr)) (Maybe Expr)
    -- ^ @reset T to (w, h)@, maybe @from p to q@, maybe
    -- @filled with v@
  | SForEach String String Stmt
    -- ^ @for each item in list: body@
  | SWithFirst String Expr String Stmt (Maybe Stmt)
    -- ^ @with first item matching p in list: body else: fallback@
  | SProc String [String] Stmt
    -- ^ @define name(x, y):@ followed by a block of code
  | SRun String (Maybe Expr)
    -- ^ @run name@, or @run name(a, b)@ (with the arguments parsed as one
    -- expression, a tuple when there are several, like a function call)
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
  SDeclTuple p e -> Set.fromList (patternNames p) <> exprNames e
  SSetTuple p e -> Set.fromList (patternNames p) <> exprNames e
  SFun f _ e -> Set.insert f (exprNames e)
  SIf c yes no -> exprNames c <> stmtNames yes <> stmtNames no
  SLoop _ c lim body -> exprNames c <> foldMap exprNames lim <> stmtNames body
  SIterate p e _ c lim -> Set.fromList (patternNames p) <> exprNames e <> exprNames c <> foldMap exprNames lim
  SPass -> Set.empty
  SSetTable n ix v -> Set.insert n (exprNames ix <> exprNames v)
  SResize n sz fill -> Set.insert n (exprNames sz <> foldMap exprNames fill)
  SAllocate n sz corners fill -> Set.insert n (exprNames sz <> foldMap (\(p, q) -> exprNames p <> exprNames q) corners
                                             <> foldMap exprNames fill)
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
  SProc name _ body -> Set.insert name (stmtNames body)
  SRun name arg -> Set.insert name (foldMap exprNames arg)

-- | The names a declaration or function definition defines.
definedNames :: Stmt -> [String]
definedNames (Stmt _ node) = case node of
  SDecl n _ _ -> [n]
  SDeclTuple p _ -> patternNames p
  SFun f _ _ -> [f]
  SProc name _ _ -> [name]
  _ -> []
