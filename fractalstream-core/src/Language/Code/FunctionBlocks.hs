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
-- A call can be anywhere in a script's code (but where 'checkStmt' says
-- it can't be): one inside an expression is first lifted out of it, into
-- an assignment of its own just before it (see 'expandStmt' and
-- 'liftExpr'), so that each call is the whole right-hand side of an
-- assignment: @x := f(a)@, @x <- f(a)@, or a tuple's, @(w, ok) := f(a)@.
-- Each call is
-- expanded, after code blocks are (see "Language.Code.Procedures", so
-- that a code block can call a function it's given) and after @previous@
-- is (see "Language.Code.Previous", so that a @previous@ belongs to the
-- loop around it where it's written, in the body or in the caller), into
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
import Language.Code.Previous (declaredIn, mapStmtRanges, mapStmtExprs)
import Language.Code.Typecheck.Functions (splitArgs, freeVars, isFunctionArg, rename, substituteFunction, applyFunction)
import Language.Type (FSType(RealT))
import Language.Code.FunctionArgs (substituteFunctionInStmt)
import Data.List (partition)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
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

  s0 : rest -> do
    s <- mapStmtExprs (fmap (withBodies (ctxDefs ctx)) . compositions (ctxDefs ctx)) s0
    lift (checkStmt (ctxDefs ctx) s)
    ss <- expandStmt ctx s
    rest' <- block (oneLine s (declare s ctx)) rest
    pure (ss ++ rest')

-- | A one-line function, @g(x) := e@, is also defined as a function with
-- a body, @g <- e@, under another name ('withBodyName'), for the calls
-- that give it a function with a body (see 'withBodies'): those are
-- expanded as calls of a function with a body, so that the calls in the
-- function it's given can be lifted.
oneLine :: Stmt -> Ctx -> Ctx
oneLine (Stmt sr node) ctx = case node of
  SFun g params e ->
    let g' = withBodyName g
        body = Stmt sr (SSet (functionValueName g') e)
    in ctx { ctxDefs = Map.insert g' (FunctionDef params body (ctxDefs ctx)) (ctxDefs ctx) }
  _ -> ctx

withBodyName :: String -> String
withBodyName g = g ++ " (given a function with a body)"

-- | Calls of one-line functions given a function with a body (by name,
-- or in a composition, or written in place, calling one), made calls of
-- their versions with a body (see 'oneLine').
withBodies :: FunctionDefs -> Expr -> Expr
withBodies defs (Syn sr node) = Syn sr $ case node of
  ECall g args
    | Map.member (withBodyName g) defs, any givesBody (argParts args) ->
        ECall (withBodyName g) (map (withBodies defs) args)
  _ -> fmap (withBodies defs) node
  where
    givesBody a@(Syn _ n) = case n of
      EFunRef f -> Map.member f defs
      ECompose{} -> any (`Map.member` defs) (composed a) || hasCall defs a
      ELambda _ b -> hasCall defs (withBodies defs b)
      _ -> False

-- | Compositions with a function with a body in them, @(f ∘ g)(x)@, made
-- nested calls, @f(g(x))@, where they're applied, so that the calls can
-- be lifted; given on to a call, a composition stays one, to be applied
-- in the body it's given to. (The rest are made functions written in
-- place when one-line calls are expanded: see
-- "Language.Code.Typecheck.Functions".)
--
-- Their derivatives, and those of functions with a body, @(f ∘ g)'(x)@
-- and @f'(x)@, are made @diff@s by a fresh variable, @t@, bound to @x@:
-- @diff(f(g(t)), t)@, which "Language.Code.Derivatives" works out
-- through the lifted calls' statements.
compositions :: FunctionDefs -> Expr -> Expand Expr
compositions defs e = do
  (e', found) <- runWriterT (pull e)
  foldM (\b (v, c) -> lift (substituteFunction v v c b)) e' found
  where
    pull (Syn sr node) = case node of
      ECompose{} | withBody (Syn sr node) -> do
        v <- freshName "composition"
        tell [(v, Syn sr node)]
        pure (Syn sr (EVar v))
      EDerivOf c order arg | withBody c -> do
        arg' <- pull arg
        derivative sr order arg' c
      EDerivCall f order args
        | Just (FunctionDef ps _ _) <- Map.lookup f defs -> case (ps, args) of
            ([_], [arg]) -> do
              arg' <- pull arg
              derivative sr order arg' (Syn sr (EFunRef f))
            -- (of several variables: the derivative in the first, f'(a, b)
            -- being diff(f(t, b), t) with t bound to a; the arguments
            -- may come as one tuple)
            (_ : _ : _, _) | Just (a1 : rest) <- spreadTo (length ps) args -> do
              a1' <- pull a1
              rest' <- traverse pull rest
              t <- freshName "derivative"
              let x = Syn sr (EVar t)
              pure (Syn sr (ELet t (Syn (synRange a1') (EWiden RealT a1'))
                                   (iterate (Syn sr . EDiff x) (Syn sr (ECall f (x : rest'))) !! order)))
            _ -> pure (Syn sr (EAdvice ("`" ++ f ++ replicate order '\'' ++ "` needs as many arguments as `"
                                        ++ f ++ "` has parameters, one for each.")))
      _ -> Syn sr <$> traverse pull node

    derivative sr order arg fn = do
      t <- freshName "derivative"
      tag <- freshName "applied"
      let x = Syn sr (EVar t)
      applied <- pull =<< lift (lift (applyFunction tag sr fn x))
      -- the variable must be at least real to differentiate by it
      pure (Syn sr (ELet t (Syn (synRange arg) (EWiden RealT arg))
                           (iterate (Syn sr . EDiff x) applied !! order)))
    withBody c = any (`Map.member` defs) (composed c) || hasCall defs c
    -- (arguments, as many as a function's parameters: given one by one,
    -- or as one tuple)
    spreadTo n = \case
      as | length as == n -> Just as
      [Syn _ (EPair a b)] | n > 1 -> (a :) <$> spreadTo (n - 1) [b]
      [a] | n == 1 -> Just [a]
      _ -> Nothing

-- | A statement whose right-hand side is a call to a function with a
-- body: how to put the statement back together around another
-- right-hand side, the function, where the call is, and its arguments.
wholeCall :: FunctionDefs -> StmtF -> Maybe (Expr -> StmtF, String, SourceRange, [Expr])
wholeCall defs = \case
  SDecl n ty e | Just (f, sr, args) <- call e -> Just (SDecl n ty, f, sr, args)
  SSet n e | Just (f, sr, args) <- call e -> Just (SSet n, f, sr, args)
  SDeclTuple p e | Just (f, sr, args) <- call e -> Just (SDeclTuple p, f, sr, args)
  SSetTuple p e | Just (f, sr, args) <- call e -> Just (SSetTuple p, f, sr, args)
  SDeclHomogeneous ns e | Just (f, sr, args) <- call e -> Just (SDeclHomogeneous ns, f, sr, args)
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
  SDeclHomogeneous ns _ -> seeing ns
  _ -> id

------------------------------------------------------
-- Calls inside expressions
------------------------------------------------------

-- | Lifting: the statements that work out the calls in an expression,
-- to run before it.
type Lift = WriterT [Stmt] Expand

-- | A statement, with every call to a function with a body in it
-- expanded: each one is lifted out of its expression, into the
-- statements just before it, and replaced by the variable holding its
-- result. 'checkStmt' has made sure there are calls only where they can
-- be lifted.
expandStmt :: Ctx -> Stmt -> Expand [Stmt]
expandStmt ctx s@(Stmt sr node) = case node of
  _ | Just (rebuild, f, callSr, args) <- wholeCall defs node -> lifted $ do
        args' <- mapM liftArg args
        result <- call callSr f args'
        pure (Stmt sr (rebuild (Syn callSr (EVar result))))

  SIf c yes no -> lifted $ do
    c' <- liftExpr ctx c
    Stmt sr <$> (SIf c' <$> inner yes <*> inner no)

  -- A loop's limit is worked out once, before the loop (as
  -- "Language.Code.Typecheck" does), and its condition before each check
  -- of it: before the first pass (unless the loop runs its body first),
  -- and at the end of each pass.
  SLoop kind c lim body
    | any (hasCall defs) lim && not (hasCall defs c) -> lifted $ do
        lim' <- traverse (liftExpr ctx) lim
        Stmt sr . SLoop kind c lim' <$> inner body
    | hasCall defs c -> lifted $ do
        lim' <- traverse (liftExpr ctx) lim
        cv <- freshName "condition"
        body' <- inner body
        initial <- if kind `elem` [LoopDoWhile, LoopDoUntil]
                   then pure (Syn sr EZero)
                   else liftExpr ctx c
        tell [Stmt sr (SDecl cv Nothing initial)]
        (c', again) <- lift (runWriterT (liftExpr ctx c))
        pure (Stmt sr (SLoop kind (Syn sr (EVar cv)) lim'
                        (Stmt sr (SBlock (body' : again ++ [Stmt sr (SSet cv c')])))))
  -- (an @iterate@ is the loop "Language.Code.Typecheck" makes of it)
  SIterate p e isWhile c lim
    | hasCall defs e || hasCall defs c || any (hasCall defs) lim ->
        expandStmt ctx $ Stmt sr $ SLoop (if isWhile then LoopWhile else LoopUntil) c lim $ Stmt sr $
          case p of
            PVar _ n -> SSet n e
            PTuple{} -> SSetTuple p e

  _ | null (ownExprs s) || not (any (hasCall defs) (ownExprs s)) -> (: []) <$> descend ctx s
    | otherwise -> lifted (mapStmtExprs (liftExpr ctx) s)
  where
    defs = ctxDefs ctx
    lifted m = do
      (s', before) <- runWriterT m
      pure (before ++ [s'])
    inner = lift . stmt ctx
    liftArg = liftArgWith (liftExpr ctx)
    call callSr f args = do
      (before, result) <- lift (expandCall defs callSr f args)
      tell before
      pure result

-- | An expression with its calls to functions with bodies lifted out (see
-- 'expandStmt'). A call that only some of the time runs (in a branch of
-- an @if ... then ... else ...@, or on the right of @and@ or @or@) is
-- lifted into an @if@ of its own, so that it runs just when it would:
--
-- > x <- if c then f(a) else b
--
-- becomes
--
-- > [internal] lifted #k := c
-- > [internal] f #j := 0
-- > if [internal] lifted #k:
-- >   (f's body, with a, giving [internal] f #j)
-- > x <- if [internal] lifted #k then [internal] f #j else b
--
-- The expression itself stays as it was, but for its calls, so it's typed
-- just as it would be.
liftExpr :: Ctx -> Expr -> Lift Expr
liftExpr ctx e@(Syn sr node)
  | not (hasCall defs e) = pure e
  | otherwise = case node of
      ECall f args | Map.member f defs -> do
        args' <- mapM (liftArgWith (liftExpr ctx)) args
        (before, result) <- lift (expandCall defs sr f args')
        tell before
        pure (Syn sr (EVar result))
      EITE c a b -> do
        cv <- bindFresh =<< liftExpr ctx c
        a' <- onlyIf (var cv) a
        b' <- onlyIfNot (var cv) b
        pure (Syn sr (EITE (var cv) a' b'))
      EBin OpAnd a b | hasCall defs b -> do
        av <- bindFresh =<< liftExpr ctx a
        Syn sr . EBin OpAnd (var av) <$> onlyIf (var av) b
      EBin OpOr a b | hasCall defs b -> do
        av <- bindFresh =<< liftExpr ctx a
        Syn sr . EBin OpOr (var av) <$> onlyIfNot (var av) b
      -- (a function written in place, applied: its parameters are
      -- declared, so that the calls in its body can be lifted)
      ELet n v body | hasCall defs body -> do
        nv <- bindFresh =<< liftExpr ctx v
        liftExpr ctx (rename (Map.singleton n nv) body)
      _ -> Syn sr <$> traverse (liftExpr ctx) node
  where
    defs = ctxDefs ctx
    var = Syn sr . EVar
    st = Stmt sr
    bindFresh v = do
      n <- freshName "lifted"
      tell [st (SDecl n Nothing v)]
      pure n
    onlyIf g = conditionally (\body -> st (SIf g body (st SPass)))
    onlyIfNot g = conditionally (\body -> st (SIf g (st SPass) body))
    -- The calls in an expression, lifted into a statement of their own
    -- that runs only some of the time. The variables they declare are
    -- used by the expression, outside that statement, so they're
    -- declared before it (as placeholders, like a call's result), and
    -- only given their values in it.
    conditionally wrap x = do
      (x', before) <- lift (runWriterT (liftExpr ctx x))
      unless (null before) $ do
        let declared = [ (n, ty, v) | Stmt _ (SDecl n ty v) <- before ]
            body = [ case b of
                       Stmt bsr (SDecl n _ v) -> Stmt bsr (SSet n v)
                       _ -> b
                   | b <- before, not (isPlaceholder b) ]
        tell [ st (SDecl n ty (Syn sr EZero)) | (n, ty, _) <- declared ]
        tell [ wrap (st (SBlock body)) ]
      pure x'
    isPlaceholder = \case
      Stmt _ (SDecl _ _ (Syn _ EZero)) -> True
      _ -> False

-- | Lift the calls in a call's arguments, but not in the functions given
-- as arguments: those are lifted where they're applied, in the body. (The
-- arguments of @f(a, b)@ are the parts of a tuple.)
liftArgWith :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
liftArgWith liftOne = go
  where
    go a@(Syn sr node) = case node of
      _ | isFunctionArg a -> pure a
      EPair x y -> (\x' y' -> Syn sr (EPair x' y')) <$> go x <*> go y
      _ -> liftOne a

-- | A call's arguments: the parts of the tuple of @f(a, b)@.
argParts :: [Expr] -> [Expr]
argParts = concatMap $ \case
  Syn _ (EPair x y) -> argParts [x, y]
  a -> [a]

-- | A fresh internal name.
freshName :: MonadState Int m => String -> m String
freshName what = do
  k <- get
  put (k + 1)
  pure ("[internal] " ++ what ++ " #" ++ show k)

-- | Whether an expression calls a function with a body.
hasCall :: FunctionDefs -> Expr -> Bool
hasCall defs = not . null . callsIn defs

-- | The calls to functions with bodies in an expression, with where each
-- is.
callsIn :: FunctionDefs -> Expr -> [(SourceRange, String)]
callsIn defs (Syn sr node) = here ++ foldMap (callsIn defs) node
  where here = case node of
          ECall f _ | Map.member f defs -> [(sr, f)]
          EDerivCall f _ _ | Map.member f defs -> [(sr, f)]
          _ -> []

-- | Make sure that a statement calls functions with bodies only where
-- the calls can be lifted out (see 'expandStmt'): not in the parts of an
-- expression worked out once for each item of a list, or under a
-- @partial@, and so on. The calls in a function given to a function
-- with a body are checked once it's put in place, in the body.
checkStmt :: FunctionDefs -> Stmt -> TC ()
checkStmt defs s@(Stmt _ node) = case node of
  SFun g _ e -> refuseIn e $ \f ->
    "`" ++ g ++ "` is defined with `:=`, so it is a single value, but it calls `" ++ f
    ++ "`, a function with a body. Make `" ++ g ++ "` a function with a body too: `function "
    ++ g ++ "(...):`, then `" ++ g ++ " <- ...` on the next line, indented."
  SWithFirst item p _ _ _ -> refuseIn p $ \f ->
    "The condition of `with first " ++ item ++ "` is checked for each item, so it can't call `"
    ++ f ++ "`, a function with a body, for now. Use a `for each` loop instead."
  _ -> mapM_ (checkExpr defs) (ownExprs s)
  where refuseIn = refuseCallsIn defs

-- | The functions, by name, that a composition is made of.
composed :: Expr -> [String]
composed (Syn _ node) = case node of
  EFunRef f -> [f]
  ECompose f g -> composed f ++ composed g
  _ -> []

-- | Make sure an expression calls functions with bodies only where the
-- calls can be lifted out (see 'checkStmt').
checkExpr :: FunctionDefs -> Expr -> TC ()
checkExpr defs (Syn _ node) = case node of
  ECall f args | Map.member f defs -> mapM_ (\a -> unless (isFunctionArg a) (checkExpr defs a)) (argParts args)
  -- (a composition given to a one-line function)
  ECall g args -> forM_ (argParts args) $ \a -> case a of
    Syn asr ECompose{} -> do
      forM_ (composed a) $ \f -> when (Map.member f defs) $ advise asr
        ("`" ++ f ++ "` is a function with a body, so a composition with it can't be given to the \
         \one-line function `" ++ g ++ "`, for now.")
      refuseIn a $ \f -> "This composition calls `" ++ f ++ "`, a function with a body, so it can't \
                         \be given to the one-line function `" ++ g ++ "`, for now."
    -- (a function written in place, given to a one-line function, is
    -- checked as it is: it can't call a function with a body)
    _ -> checkExpr defs a
  ELambda _ body -> refuseIn body $ \f ->
    "This function can't call `" ++ f ++ "`, a function with a body, for now: only a function \
    \given to a function with a body can."
  EPartial e x -> do
    refuseIn e partialAdvice
    refuseIn x partialAdvice
  ERemove l item p -> checkExpr defs l >> refuseIn p (perItem "remove" item)
  ETransform l item p -> checkExpr defs l >> refuseIn p (perItem "transform" item)
  EFind l item p d -> do
    checkExpr defs l
    refuseIn p (perItem "find" item)
    refuseIn d (perItem "find" item)
  ETablePointOr t z v -> do
    checkExpr defs t
    checkExpr defs z
    refuseIn v $ \f ->
      "The value after `else` is only used for a point off the table, so it can't call `" ++ f
      ++ "`, a function with a body, for now. Work it out first: `v := " ++ f ++ "(...)`."
  ELet _ v body -> checkExpr defs v >> checkExpr defs body
  _ -> mapM_ (checkExpr defs) node
  where
    refuseIn = refuseCallsIn defs
    partialAdvice f =
      "`partial` treats everything but its variable as a constant, so it can't see inside `" ++ f
      ++ "`, a function with a body. Use `diff`, which can."
    perItem what item f =
      "`" ++ what ++ "` works this out for each `" ++ item ++ "`, so it can't call `" ++ f
      ++ "`, a function with a body, for now. Use a `for each` loop instead."

-- | An error at the first call to a function with a body in an
-- expression, if there is one.
refuseCallsIn :: FunctionDefs -> Expr -> (String -> String) -> TC ()
refuseCallsIn defs e msg = case callsIn defs e of
  (sr, f) : _ -> advise sr (msg f)
  [] -> pure ()

-- | The expressions a statement has of its own (not those of the
-- statements inside it).
ownExprs :: Stmt -> [Expr]
ownExprs (Stmt _ node) = case node of
  SDecl _ _ e -> [e]
  SSet _ e -> [e]
  SDeclTuple _ e -> [e]
  SSetTuple _ e -> [e]
  SDeclHomogeneous _ e -> [e]
  SFun _ _ e -> [e]
  SIf c _ _ -> [c]
  SLoop _ c lim _ -> c : maybeToList lim
  SIterate _ e _ c lim -> e : c : maybeToList lim
  SDraw d -> case d of
    DrawPointAt p -> [p]
    DrawCircleAt _ c r _ -> [c, r]
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
  -- calls to other functions with bodies: only where they can be lifted
  mapM_ (checkStmt (ctxDefs ctx)) (allStmts body)
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
  FunctionDef params body0 bodyDefs <-
    maybe (throwError (MissingFunction sr f)) pure (Map.lookup f defs)
  -- (the parts of the body with no source range, such as the standard
  -- library's, are said to be at the call)
  let body = mapStmtRanges (\case { NoSourceRange -> sr; r -> r }) body0
  k <- get
  put (k + 1)
  (spread, args') <- lift (splitArgs sr f (length params) ("[internal] " ++ f ++ ".tuple #" ++ show k) args)
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
      run = st (SBlock ([ st (SDecl n Nothing a) | (n, a) <- spread ]
                        ++ [ st (SDecl (tag p) Nothing a) | (p, a) <- valArgs ] ++ [body']))
  pure ( st (SDecl result Nothing (Syn sr EZero))
           : (if hasLoops then save ++ [run] ++ restore else [run])
       , result )
