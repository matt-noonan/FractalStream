-- | Shared blocks of code: @define name(x, y):@ followed by a block, run
-- with @run name(a, b)@. Each @run@ is expanded, before inference and
-- elaboration, into a block that declares a fresh variable for each
-- parameter (initialized to the argument, with its type inferred as for
-- @x := e@) followed by the code, with the parameters renamed. So
-- parameters behave like local variables holding the arguments, and other
-- names in the code are resolved, and typechecked, where it is run.
--
-- A definition scopes over the rest of its block, and code can only run
-- the blocks defined before it (so never itself).
module Language.Code.Procedures
  ( expandRuns
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Typecheck
import Language.Parser.SourceRange
import Language.Code.Syntax
import Language.Value.Syntax (Expr)
import Language.Code.Typecheck.Functions (splitArgs, rename)

-- | A block of code: its parameters, its body, and the blocks of code that
-- were defined where it was.
data ProcDef = ProcDef [String] Stmt Procs

type Procs = Map String ProcDef

type Expand = StateT Int TC

-- | Expand every @run@, and drop the definitions.
expandRuns :: Stmt -> TC Stmt
expandRuns s = evalStateT (stmt Map.empty s) 0

stmt :: Procs -> Stmt -> Expand Stmt
stmt procs (Stmt sr node) = Stmt sr <$> case node of
  SBlock body -> SBlock <$> block procs body
  -- a definition only makes sense as part of a block
  SProc{} -> SBlock <$> block procs [Stmt sr node]
  SRun name arg -> run procs sr name arg
  SIf c yes no -> SIf c <$> stmt procs yes <*> stmt procs no
  SLoop kind c lim body -> SLoop kind c lim <$> stmt procs body
  SForEach item list body -> SForEach item list <$> stmt procs body
  SWithFirst item p list body fallback ->
    SWithFirst item p list <$> stmt procs body <*> traverse (stmt procs) fallback
  _ -> pure node

block :: Procs -> [Stmt] -> Expand [Stmt]
block procs = \case
  [] -> pure []
  Stmt sr (SProc name params body) : rest -> do
    when (Map.member name procs) $ throwError (AlreadyDefined sr name)
    block (Map.insert name (ProcDef params body procs) procs) rest
  s : rest -> (:) <$> stmt procs s <*> block procs rest

run :: Procs -> SourceRange -> String -> Maybe Expr -> Expand StmtF
run procs sr name arg = do
  ProcDef params body defProcs <-
    maybe (throwError (MissingCode sr name)) pure (Map.lookup name procs)
  args <- case (params, arg) of
    ([], Nothing) -> pure []
    ([], Just _) -> advise sr (name ++ " takes no arguments.")
    _ -> lift (splitArgs sr name (length params) (maybeToList arg))
  k <- get
  put (k + 1)
  let names = [ "[internal] " ++ name ++ "." ++ p ++ " #" ++ show k | p <- params ]
  body' <- stmt defProcs (renameStmt (Map.fromList (zip params names)) body)
  pure (SBlock ([ Stmt sr (SDecl n Nothing a) | (n, a) <- zip names args ] ++ [body']))

-- | Rename variables, wherever they occur.
renameStmt :: Map String String -> Stmt -> Stmt
renameStmt m (Stmt sr node) = Stmt sr $ case node of
  SBlock body -> SBlock (map go body)
  SDecl n ty e -> SDecl (var n) ty (expr e)
  SSet n e -> SSet (var n) (expr e)
  SFun f params e -> SFun f params (rename (Map.withoutKeys m (Set.fromList params)) e)
  SIf c yes no -> SIf (expr c) (go yes) (go no)
  SLoop kind c lim body -> SLoop kind (expr c) (expr <$> lim) (go body)
  SIterate n e isWhile c lim -> SIterate (var n) (expr e) isWhile (expr c) (expr <$> lim)
  SPass -> SPass
  SDraw d -> SDraw $ case d of
    DrawPointAt p -> DrawPointAt (expr p)
    DrawCircleAt f c r -> DrawCircleAt f (expr c) (expr r)
    DrawRectFrom f a b -> DrawRectFrom f (expr a) (expr b)
    DrawLineFrom a b -> DrawLineFrom (expr a) (expr b)
    UseStroke c -> UseStroke (expr c)
    UseFill c -> UseFill (expr c)
    EraseAll -> EraseAll
    WriteAt t p -> WriteAt (expr t) (expr p)
  SForEach item list body -> SForEach (var item) (var list) (go body)
  SWithFirst item p list body fallback ->
    SWithFirst (var item) (expr p) (var list) (go body) (go <$> fallback)
  SProc name params body ->
    SProc name params (renameStmt (Map.withoutKeys m (Set.fromList params)) body)
  SRun name arg -> SRun name (expr <$> arg)
  where
    go = renameStmt m
    expr = rename m
    var n = Map.findWithDefault n n m
