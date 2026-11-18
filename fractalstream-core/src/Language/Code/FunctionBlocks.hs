-- | Functions with a body: @function f(x, y):@ followed by a block, which
-- gives the function its value by assigning to its name (@f <- ...@), on
-- every path through it.
--
-- A function is pure: its body works out a value, from its parameters,
-- its own variables, and the names the project gives it (the
-- configuration, @\@common@'s definitions, the viewer's or tool's
-- variables, and the built-in ones, like @pixel_size@). It can't draw,
-- change anything but its own variables, write a table, or run a code
-- block, and its loops don't change the caller's @iterations@ or @stuck@.
--
-- For now a call is the whole right-hand side of an assignment:
-- @x := f(a)@, @x <- f(a)@, or a tuple's, @(w, ok) := f(a)@. Each call is
-- expanded, after code blocks are (see "Language.Code.Procedures", so
-- that a code block can call a function it's given), into
--
-- > [internal] f #k := (a placeholder, of the type the body gives f)
-- > (the caller's iterations and stuck, saved, if the body has loops)
-- > block:
-- >   (each parameter, a fresh variable holding its argument)
-- >   (the body, with its parameters, its variables, and f renamed)
-- > (iterations and stuck, put back)
-- > x := [internal] f #k
--
-- so that the body's variables can have any names, even ones the caller
-- uses. The placeholder ('EZero') takes the type that inference finds for
-- the result from the body's assignments.
module Language.Code.FunctionBlocks
  ( expandFunctionBlocks
  , bodyUses
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Typecheck
import Language.Parser.SourceRange
import Language.Code.Syntax
import Language.Value.Syntax
import Language.Value.Builtins (internalIterations, internalStuck)
import Language.Code.Procedures (renameStmt)
import Language.Code.Previous (declaredIn)
import Language.Code.Typecheck.Functions (splitArgs, freeVars, isFunctionArg)
import Language.Code.FunctionArgs (substituteFunctionInStmt)
import Data.List (partition)
import Language.Parser (functionValueName)

-- | A function with a body: its parameters, its body, and the functions
-- defined where it was (the ones its body can call).
data FunctionDef = FunctionDef [String] Stmt FunctionDefs

type FunctionDefs = Map String FunctionDef

-- | What's known where a statement is: the functions defined so far, and
-- the names in scope (the environment's, and those declared so far).
data Ctx = Ctx
  { ctxDefs    :: FunctionDefs
  , ctxVisible :: Set String
  }

type Expand = StateT Int TC

-- | Check every function definition, expand every call, and drop the
-- definitions. The set is the names of the environment the code runs in.
expandFunctionBlocks :: Set String -> Stmt -> TC Stmt
expandFunctionBlocks envNames s = evalStateT (stmt (Ctx Map.empty envNames) s) 0

stmt :: Ctx -> Stmt -> Expand Stmt
stmt ctx s@(Stmt sr node) = case node of
  SBlock body -> Stmt sr . SBlock <$> block ctx body
  _ -> block ctx [s] <&> \case
    [one] -> one
    many' -> Stmt sr (SBlock many')

block :: Ctx -> [Stmt] -> Expand [Stmt]
block ctx = \case
  [] -> pure []

  Stmt sr (SFunction f params body) : rest -> do
    when (Map.member f (ctxDefs ctx)) $ throwError (AlreadyDefined sr f)
    lift (checkDefinition ctx sr f params body)
    block ctx { ctxDefs = Map.insert f (FunctionDef params body (ctxDefs ctx)) (ctxDefs ctx) } rest

  s@(Stmt sr node) : rest -> case wholeCall (ctxDefs ctx) node of
    Just (rebuild, f, callSr, args) -> do
      (before, result) <- expandCall (ctxDefs ctx) callSr f args
      let s' = Stmt sr (rebuild (Syn callSr (EVar result)))
      rest' <- block (declare s' ctx) rest
      pure (before ++ [s'] ++ rest')
    Nothing -> do
      lift (noOtherCalls (ctxDefs ctx) s)
      s' <- descend ctx s
      rest' <- block (declare s ctx) rest
      pure (s' : rest')

-- | A statement whose right-hand side is a call to a function with a
-- body: how to put the statement back together around another
-- right-hand side, the function, where the call is, and its arguments.
wholeCall :: FunctionDefs -> StmtF -> Maybe (Expr -> StmtF, String, SourceRange, [Expr])
wholeCall defs = \case
  SDecl n ty e | Just (f, sr, args) <- call e -> Just (SDecl n ty, f, sr, args)
  SSet n e | Just (f, sr, args) <- call e -> Just (SSet n, f, sr, args)
  SDeclTuple p e | Just (f, sr, args) <- call e -> Just (SDeclTuple p, f, sr, args)
  SSetTuple p e | Just (f, sr, args) <- call e -> Just (SSetTuple p, f, sr, args)
  _ -> Nothing
  where
    call = \case
      Syn sr (ECall f args) | Map.member f defs -> Just (f, sr, args)
      _ -> Nothing

-- | The statements inside a statement, expanded in turn.
descend :: Ctx -> Stmt -> Expand Stmt
descend ctx (Stmt sr node) = Stmt sr <$> case node of
  SBlock body -> SBlock <$> block ctx body
  SIf c yes no -> SIf c <$> stmt ctx yes <*> stmt ctx no
  SLoop kind c lim body -> SLoop kind c lim <$> stmt ctx body
  SForEach item list body -> SForEach item list <$> stmt (seeing [item] ctx) body
  SWithFirst item p list body fallback ->
    SWithFirst item p list <$> stmt (seeing [item] ctx) body <*> traverse (stmt ctx) fallback
  SProc name params body -> SProc name params <$> stmt (seeing params ctx) body
  _ -> pure node

seeing :: [String] -> Ctx -> Ctx
seeing names ctx = ctx { ctxVisible = ctxVisible ctx <> Set.fromList names }

-- | The context after a statement: with the variables it declares.
declare :: Stmt -> Ctx -> Ctx
declare (Stmt _ node) = case node of
  SDecl n _ _ -> seeing [n]
  SDeclTuple p _ -> seeing (patternNames p)
  _ -> id

-- | A call to a function with a body anywhere else than as the whole
-- right-hand side of an assignment is an error, for now.
noOtherCalls :: FunctionDefs -> Stmt -> TC ()
noOtherCalls defs s = forM_ (ownExprs s) $ \e ->
  case [ (sr, f) | (sr, f) <- calls e, Map.member f defs ] of
    (sr, f) : _ -> advise sr
      ("`" ++ f ++ "` is a function with a body, which can only be called as the whole \
       \right-hand side of an assignment for now: `x := " ++ f ++ "(...)`, `x <- "
       ++ f ++ "(...)`, or `(x, y) := " ++ f ++ "(...)`.")
    [] -> pure ()
  where
    calls (Syn sr node) = here ++ foldMap calls node
      where here = case node of
              ECall f _ -> [(sr, f)]
              EDerivCall f _ _ -> [(sr, f)]
              _ -> []

-- | The expressions a statement has of its own (not those of the
-- statements inside it).
ownExprs :: Stmt -> [Expr]
ownExprs (Stmt _ node) = case node of
  SDecl _ _ e -> [e]
  SSet _ e -> [e]
  SDeclTuple _ e -> [e]
  SSetTuple _ e -> [e]
  SFun _ _ e -> [e]
  SIf c _ _ -> [c]
  SLoop _ c lim _ -> c : maybeToList lim
  SIterate _ e _ c lim -> e : c : maybeToList lim
  SDraw d -> case d of
    DrawPointAt p -> [p]
    DrawCircleAt _ c r _ -> [c, r]
    DrawGeodesicFrom a b -> [a, b]
    DrawRectFrom _ a b -> [a, b]
    DrawLineFrom a b -> [a, b]
    UseStroke c -> [c]
    UseFill c -> [c]
    EraseAll -> []
    WriteAt t p -> [t, p]
  SSetTable _ ix v -> [ix, v]
  SResize _ sz fill -> sz : maybeToList fill
  SAllocate _ sz corners fill -> sz : maybe [] (\(p, q) -> [p, q]) corners ++ maybeToList fill
  SWithFirst _ p _ _ _ -> [p]
  SRun _ arg -> maybeToList arg
  _ -> []

-- | Every statement in a statement, itself included.
allStmts :: Stmt -> [Stmt]
allStmts s@(Stmt _ node) = s : case node of
  SBlock body -> concatMap allStmts body
  SIf _ yes no -> allStmts yes ++ allStmts no
  SLoop _ _ _ body -> allStmts body
  SForEach _ _ body -> allStmts body
  SWithFirst _ _ _ body fallback -> allStmts body ++ foldMap allStmts fallback
  SProc _ _ body -> allStmts body
  SFunction _ _ body -> allStmts body
  _ -> []

------------------------------------------------------
-- Definitions
------------------------------------------------------

checkDefinition :: Ctx -> SourceRange -> String -> [String] -> Stmt -> TC ()
checkDefinition ctx sr f params body = do
  when (length (Set.fromList params) /= length params) $
    advise sr ("The parameters of " ++ f ++ " must all have different names.")
  let own = Set.fromList params <> declaredIn body
      value = functionValueName f
  when (Set.member value own) $
    advise sr ("`" ++ f ++ "` names the function, so its parameters and variables can't also be called `"
               ++ f ++ "`.")
  -- what the body may do
  forM_ (allStmts body) $ \(Stmt ssr node) -> case node of
    SDraw{} -> advise ssr (f ++ " can't draw: a function only works out a value. \
                              \Draw in the tool that calls it.")
    SSetTable n _ _ -> changesTable ssr n
    SResize n _ _ -> changesTable ssr n
    SAllocate n _ _ _ -> changesTable ssr n
    SRun name _ -> advise ssr (f ++ " can't run the code block `" ++ name ++ "`, which could \
                               \draw, or change what isn't " ++ f ++ "'s own. Make it a function instead.")
    SFunction g _ _ -> nested ssr g
    SFun g _ _ -> nested ssr g
    SProc g _ _ -> nested ssr g
    SSet n _ -> assigns ssr own [n]
    SSetTuple p _ -> assigns ssr own (patternNames p)
    SIterate p _ _ _ _ -> assigns ssr own (patternNames p)
    _ -> pure ()
  -- what it may see: its own names, and what's in scope where it's defined
  forM_ (Map.toList (bodyUses f params body `Map.withoutKeys` ctxVisible ctx)) $ \(n, usr) ->
    throwError (MissingName usr n)
  -- calls to other functions with bodies: only as whole right-hand sides
  forM_ (allStmts body) $ \s@(Stmt _ node) -> case wholeCall (ctxDefs ctx) node of
    Just _ -> pure ()
    Nothing -> noOtherCalls (ctxDefs ctx) s
  -- every path gives f a value, and nothing reads it before then
  given <- definitelyAssigns f value False body
  unless given $ advise sr
    ("`" ++ f ++ "` isn't given a value on every path through its body. Give it one with `"
     ++ f ++ " <- ...` on each path: in every branch of an `if` (with an `else`), and not \
     \only inside a loop, which might not run.")
  where
    changesTable ssr n = advise ssr (f ++ " can't change the table `" ++ n ++ "`: a function only \
                                     \works out a value. Change it in the tool that calls " ++ f ++ ".")
    nested ssr g = advise ssr ("A function's body can't define `" ++ g ++ "`: define it before "
                               ++ f ++ ", outside it.")
    assigns ssr own names = forM_ names $ \n ->
      unless (n == functionValueName f || Set.member n own) $ advise ssr
        (f ++ " can't change `" ++ n ++ "`, which isn't one of its parameters or variables: \
         \a function only works out a value (its own, by `" ++ f ++ " <- ...`).")

-- | The names a function's body uses that aren't its own (its parameters,
-- its variables, or its value), with where one use of each is.
bodyUses :: String -> [String] -> Stmt -> Map String SourceRange
bodyUses f params body = used `Map.withoutKeys` own
  where
    own = Set.fromList (functionValueName f : params) <> declaredIn body
    used = Map.unions (map freeVars (concatMap ownExprs (allStmts body)))
           <> Map.fromList [ (list, lsr) | Stmt lsr node <- allStmts body
                                         , list <- case node of
                                             SForEach _ l _ -> [l]
                                             SWithFirst _ _ l _ _ -> [l]
                                             _ -> [] ]

-- | Whether a statement certainly assigns @f@ (given whether it already
-- was), with an error where it's read before it certainly was. A loop's
-- body might not run, so what it assigns doesn't count after it.
definitelyAssigns :: String -> String -> Bool -> Stmt -> TC Bool
definitelyAssigns f value = go
  where
    go assigned s@(Stmt _ node) = case node of
      SBlock body -> foldM go assigned body
      SSet n e -> uses assigned [e] $> (assigned || n == value)
      SSetTuple p e -> uses assigned [e] $> (assigned || value `elem` patternNames p)
      SIf c yes no -> do
        uses assigned [c]
        (&&) <$> go assigned yes <*> go assigned no
      SLoop kind c lim body -> do
        uses assigned (maybeToList lim)
        if kind `elem` [LoopDoWhile, LoopDoUntil]
          then go assigned body >>= \a -> uses a [c]
          else uses assigned [c] >> void (go assigned body)
        pure assigned
      SForEach _ _ body -> go assigned body $> assigned
      SWithFirst _ p _ body fallback -> do
        uses assigned [p]
        inBody <- go assigned body
        case fallback of
          Just fb -> (inBody &&) <$> go assigned fb
          Nothing -> pure assigned
      _ -> uses assigned (ownExprs s) $> assigned

    uses assigned es = unless assigned $
      forM_ (take 1 (mapMaybe (Map.lookup value . freeVars) es)) $ \usr ->
        advise usr ("`" ++ f ++ "` is read here before its body has given it a value.")

------------------------------------------------------
-- Calls
------------------------------------------------------

-- | The statements a call expands into, before the assignment that uses
-- its result, and the name of the variable holding the result.
expandCall :: FunctionDefs -> SourceRange -> String -> [Expr] -> Expand ([Stmt], String)
expandCall defs sr f args = do
  FunctionDef params body bodyDefs <-
    maybe (throwError (MissingFunction sr f)) pure (Map.lookup f defs)
  args' <- lift (splitArgs sr f (length params) args)
  k <- get
  put (k + 1)
  -- (a parameter given a function is put in place, applied, in the body,
  -- after the body's own names are renamed, so that a function written in
  -- place keeps the caller's: see "Language.Code.FunctionArgs")
  let (fnArgs, valArgs) = partition (isFunctionArg . snd) (zip params args')
      tag n = "[internal] " ++ f ++ "." ++ n ++ " #" ++ show k
      result = "[internal] " ++ f ++ " #" ++ show k
      names = Map.fromList ((functionValueName f, result)
                            : [ (n, tag n) | n <- map fst valArgs ++ Set.toList (declaredIn body) ])
      st = Stmt sr
      var = Syn sr . EVar
  body1 <- lift (foldM (\b (p, a) -> substituteFunctionInStmt (f ++ " #" ++ show k) p a b)
                       (renameStmt names body) fnArgs)
  -- (a function given by name can be one defined where the call is)
  body' <- stmt (Ctx (bodyDefs `Map.union` defs) Set.empty) body1
  let hasLoops = not (null [ () | Stmt _ node <- allStmts body
                                , case node of { SLoop{} -> True; SIterate{} -> True; _ -> False } ])
      savedIterations = tag "iterations"
      savedStuck = tag "stuck"
      save = [ st (SDecl savedIterations Nothing (var internalIterations))
             , st (SDecl savedStuck Nothing (var internalStuck)) ]
      restore = [ st (SSet internalIterations (var savedIterations))
                , st (SSet internalStuck (var savedStuck)) ]
      run = st (SBlock ([ st (SDecl (tag p) Nothing a) | (p, a) <- valArgs ] ++ [body']))
  pure ( st (SDecl result Nothing (Syn sr EZero))
           : (if hasLoops then save ++ [run] ++ restore else [run])
       , result )
