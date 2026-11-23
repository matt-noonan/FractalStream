-- | User-defined functions (@f(x, y) := e@), and their expansion:
-- each call is replaced by local bindings of the (freshly renamed)
-- parameters around a copy of the body, so that a function is effectively
-- typechecked anew at each call, at the argument types used there.
module Language.Code.Typecheck.Functions
  ( Funs
  , FunDef(..)
  , defineFunction
  , expandCalls
  , splitArgs
  , rename
  , freeVars
  , isFunctionArg
  , substituteFunction
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Typecheck
import Language.Environment (EnvironmentProxy)
import Language.Value.Syntax
import Language.Value.Typing (typeEnvOf, TypeEnv(..))
import Language.Parser.SourceRange

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (partition)

-- | A user-defined function: its parameter names, its body, and the
-- functions that were in scope where it was defined.
data FunDef = FunDef
  { fdParams :: [String]
  , fdBody   :: Expr
  , fdFuns   :: Funs
  }

type Funs = Map String FunDef

------------------------------------------------------
-- Functions
------------------------------------------------------

defineFunction :: SourceRange -> Funs -> EnvironmentProxy env
               -> String -> [String] -> Expr -> TC Funs
defineFunction sr funs env f params body = do
  let tenv = tyVars (typeEnvOf env)
  when (Map.member f tenv || Map.member f funs) $
    throwError (AlreadyDefined sr f)
  when (length (Set.fromList params) /= length params) $
    advise sr ("The parameters of " ++ f ++ " must all have different names.")
  -- Everything the body refers to must already be in scope here.
  forM_ (Map.toList (freeVars body `Map.withoutKeys` Set.fromList params)) $ \(n, vsr) ->
    unless (Map.member n tenv) $ throwError (MissingName vsr n)
  -- (a parameter differentiated, `g'(x)`, is given its function at each call)
  forM_ (Map.toList (calledFunctions body `Map.withoutKeys` Set.fromList params)) $ \(g, gsr) ->
    unless (Map.member g funs) $ throwError (MissingFunction gsr g)
  pure (Map.insert f (FunDef params body funs) funs)

-- | Replace each call to a user-defined function with local bindings of
-- its (freshly renamed) parameters around a copy of its body, and each
-- derivative call @f'(a)@ with @diff(f(t), t)@ where @t@ is a fresh
-- variable bound to @a@. A composition, @f ∘ g@, is first made a function
-- written in place, @w -> f(g(w))@ (see 'compositions').
expandCalls :: Funs -> Expr -> TC Expr
expandCalls funs0 e0 = evalStateT (expand funs0 e0) (0 :: Int)
  where
    expand funs e = compositions funs e >>= go funs

    go funs (Syn sr node) = case node of
      ECall f args -> do
        (params, body0, bodyFuns) <- lookupFun sr f funs
        let body = atCallSite sr body0
        -- (the call's own number, which its parameters' names take next;
        -- a spread tuple's name is different from theirs: see 'spreadName')
        tuple <- gets (\k -> "[internal] " ++ f ++ " #" ++ show k)
        (spread, args0) <- lift (splitArgs sr f (length params) tuple args)
        spread' <- traverse (traverse (go funs)) spread
        args' <- traverse (go funs) args0
        -- (a parameter given a function is put in place, applied, in the
        -- body: see "Language.Code.FunctionArgs")
        let (fnArgs, valArgs) = partition (isFunctionArg . snd) (zip params args')
        fnArgs' <- forM fnArgs $ \(p, a) -> (p,) <$> closed f funs a
        names <- freshNames f (map fst valArgs)
        tag <- if null fnArgs then pure "" else freshName f "function"
        body1 <- lift (foldM (\b (p, a) -> substituteFunction tag p a b)
                             (rename (Map.fromList (zip (map fst valArgs) names)) body) fnArgs')
        body' <- expand bodyFuns body1
        pure (foldr (\(n, a) b -> Syn sr (ELet n a b)) body' (spread' ++ zip names (map snd valArgs)))

      EDerivCall f order args -> do
        (params, body0, bodyFuns) <- lookupFun sr f funs
        let body = atCallSite sr body0
            f' = f ++ replicate order '\''
        param <- case params of
          [p] -> pure p
          p : _ -> advise sr ("Only a function of one variable can be differentiated with "
                              ++ f' ++ ". To differentiate " ++ f
                              ++ " with respect to one of its arguments, use diff instead, like "
                              ++ "diff(" ++ f ++ "(" ++ intercalate ", " params ++ "), "
                              ++ p ++ ").")
          [] -> throwError (Internal (Advice sr (f ++ " has no parameters")))
        arg <- lift (splitArgs sr f 1 "" args) >>= \case
          (_, [a]) -> go funs a
          _ -> throwError (Internal (Advice sr "splitArgs: wrong number of arguments"))
        t <- freshName f' param
        let x = Syn sr (EVar t)
        body' <- expand bodyFuns (rename (Map.singleton param t) body)
        -- the variable must be at least real to differentiate by it
        pure (Syn sr (ELet t (Syn (synRange arg) (EWiden RealT arg))
                             (iterate (Syn sr . EDiff x) body' !! order)))

      _ -> Syn sr <$> traverse (go funs) node

    -- a function given as an argument, with the calls in it expanded
    -- where it's given (a function given by name, as its own definition)
    closed f funs a@(Syn asr node) = case node of
      EFunRef g -> case Map.lookup g funs of
        Just (FunDef ps b gFuns) -> (\b' -> Syn asr (ELambda ps b')) <$> expand gFuns b
        Nothing -> lift (advise asr ("`" ++ g ++ "` can't be given to the one-line function `" ++ f
                                     ++ "`: give it a one-line function, or write one in place (`w -> ...`)."))
      ELambda ps b -> Syn asr . ELambda ps <$> go funs b
      ECompose{} -> closed f funs =<< asFunction funs a
      _ -> pure a

    -- `f ∘ g` (and `(f ∘ g)'(x)`), each made a function written in place,
    -- and put where it's applied or given on, as for a parameter given a
    -- function (see 'substituteFunction')
    compositions funs e = do
      (e', found) <- runStateT (pull e) []
      foldM (\b (v, c) -> do
                fn <- asFunction funs c
                lift (substituteFunction v v fn b))
            e' (reverse found)
      where
        pull (Syn sr node) = case node of
          ECompose{} -> Syn sr . EVar <$> named (Syn sr node)
          EDerivOf c@(Syn _ ECompose{}) order arg -> do
            v <- named c
            Syn sr . EDerivCall v order . (:[]) <$> pull arg
          _ -> Syn sr <$> traverse pull node
        named c = do
          v <- lift (freshName "∘" "composition")
          modify ((v, c) :)
          pure v

    -- a function: by its name, written in place, or composed, as one
    -- written in place, with parameters of its own
    asFunction funs (Syn sr node) = case node of
      EFunRef g -> do
        (ps, _, _) <- lookupFun sr g funs
        ns <- traverse (freshName g) ps
        pure (Syn sr (ELambda ns (Syn sr (ECall g (map (Syn sr . EVar) ns)))))
      ELambda ps b -> do
        ns <- traverse (freshName "λ") ps
        pure (Syn sr (ELambda ns (rename (Map.fromList (zip ps ns)) b)))
      ECompose f g -> asFunction funs g >>= \case
        Syn _ (ELambda ps inner) -> do
          outer <- asFunction funs f
          tag <- freshName "∘" "applied"
          applied <- lift (applyFunction tag sr outer inner)
          pure (Syn sr (ELambda ps applied))
        _ -> throwError (Internal (Advice sr "asFunction: not a function"))
      EVar n -> lift (advise sr ("`" ++ n ++ "` isn't a function here, so it can't be composed with `∘`."))
      _ -> lift (advise sr "Only functions can be composed with `∘`.")

    lookupFun sr f funs = case Map.lookup f funs of
      Just (FunDef params body bodyFuns) -> pure (params, body, bodyFuns)
      Nothing -> throwError (MissingFunction sr f)

    -- fresh names for a function's parameters, for one expansion of a call
    freshNames f params = do
      k <- get
      put (k + 1)
      pure [ "[internal] " ++ f ++ "." ++ p ++ " #" ++ show k | p <- params ]

    freshName f param = do
      k <- get
      put (k + 1)
      pure ("[internal] " ++ f ++ "." ++ param ++ " #" ++ show k)

-- | A call @f(a, b, c)@ is parsed as @f@ applied to the tuple @(a, b, c)@;
-- split the tuple according to the number of parameters. A tuple that
-- isn't written out, such as @g(x)@'s for a @g@ that gives a pair, is
-- given the name @tuple@, and split into its parts: the binding comes
-- back too, to go before the arguments.
splitArgs :: SourceRange -> String -> Int -> String -> [Expr] -> TC ([(String, Expr)], [Expr])
splitArgs sr f n tuple = \case
  [arg] -> go n arg
  args | length args == n -> pure ([], args)
       | otherwise -> wrongCount
  where
    go 1 arg = pure ([], [arg])
    go k (Syn _ (EPair x rest)) = fmap (x :) <$> go (k - 1) rest
    go k arg@(Syn asr _)
      | isFunctionArg arg || isLiteral arg = wrongCount
      | otherwise = pure ([(spreadName tuple, arg)], parts k (Syn asr (EVar (spreadName tuple))))
      where isLiteral (Syn _ node) = case node of { ELit{} -> True; _ -> False }
            parts 1 t = [t]
            parts j t@(Syn tsr _) = Syn tsr (EProj False t) : parts (j - 1) (Syn tsr (EProj True t))
    wrongCount = advise sr (f ++ " takes " ++ show n ++ " argument" ++ (if n == 1 then "" else "s") ++ ".")

-- | A function's body, where it's called: the parts of it with no
-- source range (such as the standard library's) are said to be at the
-- call.
atCallSite :: SourceRange -> Expr -> Expr
atCallSite sr = fmap (\case { NoSourceRange -> sr; r -> r })

-- | Rename free variables.
rename :: Map String String -> Expr -> Expr
rename m (Syn sr node) = Syn sr $ case node of
  EVar n -> EVar (Map.findWithDefault n n m)
  ERemove lst n p -> ERemove (rename m lst) n (rename (Map.delete n m) p)
  EFind lst n p d -> EFind (rename m lst) n (rename (Map.delete n m) p) (rename m d)
  ETransform lst n f -> ETransform (rename m lst) n (rename (Map.delete n m) f)
  ELet n v body -> ELet n (rename m v) (rename (Map.delete n m) body)
  ELambda ps body -> ELambda ps (rename (Map.withoutKeys m (Set.fromList ps)) body)
  _ -> fmap (rename m) node

-- | Free variables, with the location of one use of each.
freeVars :: Expr -> Map String SourceRange
freeVars (Syn sr node) = case node of
  EVar n -> Map.singleton n sr
  ERemove lst n p -> freeVars lst <> Map.delete n (freeVars p)
  EFind lst n p d -> freeVars lst <> Map.delete n (freeVars p) <> freeVars d
  ETransform lst n f -> freeVars lst <> Map.delete n (freeVars f)
  ELet n v body -> freeVars v <> Map.delete n (freeVars body)
  ELambda ps body -> freeVars body `Map.withoutKeys` Set.fromList ps
  _ -> foldMap freeVars node

calledFunctions :: Expr -> Map String SourceRange
calledFunctions (Syn sr node) = case node of
  ECall f args -> Map.singleton f sr <> foldMap calledFunctions args
  EDerivCall f _ args -> Map.singleton f sr <> foldMap calledFunctions args
  EFunRef f -> Map.singleton f sr
  _ -> foldMap calledFunctions node


------------------------------------------------------
-- Functions given as arguments (see "Language.Code.FunctionArgs")
------------------------------------------------------

-- | Is this argument a function (by name, or written in place)?
isFunctionArg :: Expr -> Bool
isFunctionArg (Syn _ node) = case node of
  EFunRef{} -> True
  ELambda{} -> True
  ECompose{} -> True
  _ -> False

-- | Apply a function (by name, or written in place) to an argument; the
-- tag makes the names it binds different from those of any other
-- application.
applyFunction :: String -> SourceRange -> Expr -> Expr -> TC Expr
applyFunction tag sr fn arg = case fn of
  Syn _ (EFunRef f) -> pure (Syn sr (ECall f [arg]))
  Syn _ (ELambda ps body) -> do
    let fresh q = "[internal] " ++ tag ++ "." ++ q ++ " " ++ position sr
        names = map fresh ps
        body' = rename (Map.fromList (zip ps names)) body
    (spread, args) <- splitArgs sr "this function" (length ps) (fresh "tuple") [arg]
    pure (foldr (\(n, a) b -> Syn sr (ELet n a b)) body' (spread ++ zip names args))
  -- (`(f ∘ g)(x)` is `f(g(x))`)
  Syn _ (ECompose f g) -> applyFunction (tag ++ "∘") sr f =<< applyFunction (tag ++ "∘'") sr g arg
  _ -> throwError (Internal (Advice sr "applyFunction: not a function"))

position :: SourceRange -> String
position = \case
  SourceRange (Pos r c) _ -> "@" ++ show r ++ ":" ++ show c
  NoSourceRange -> "@?"

-- | Put the function @fn@ in place of the parameter @p@, in an expression:
-- applied, wherever @p@ is applied to something, and as it is elsewhere
-- (so that it can be given on to another call).
substituteFunction :: String -> String -> Expr -> Expr -> TC Expr
substituteFunction tag p fn = go
  where
    go e@(Syn sr node) = case node of
      EBin OpMul l r -> applied l r >>= \case
        Just e' -> pure e'
        Nothing -> Syn sr <$> traverse go node
      EVar n | n == p -> pure fn
      EDerivCall n order args | n == p -> traverse go args >>= derivOf sr order
      -- (where p is bound again, it's something else)
      ELambda ps _ | p `elem` ps -> pure e
      ELet n v body | n == p -> (\v' -> Syn sr (ELet n v' body)) <$> go v
      ERemove lst n q | n == p -> (\lst' -> Syn sr (ERemove lst' n q)) <$> go lst
      EFind lst n q d | n == p -> (\lst' d' -> Syn sr (EFind lst' n q d')) <$> go lst <*> go d
      ETransform lst n q | n == p -> (\lst' -> Syn sr (ETransform lst' n q)) <$> go lst
      _ -> Syn sr <$> traverse go node

    -- `p x`, `a p x`, `p(x)^2`: products whose left side ends in p, which
    -- are calls of p (at the far right of the left side: the products of
    -- a juxtaposition nest to the left)
    applied l r = case l of
      Syn lsr (EVar n) | n == p -> Just <$> applyTo lsr r
      Syn lsr (EBin OpMul a b) -> applied b r >>= \case
        Just b' -> (\a' -> Just (Syn lsr (EBin OpMul a' b'))) <$> go a
        Nothing -> pure Nothing
      _ -> pure Nothing

    -- `p(x)^2` parses as `p ((x)^2)`, since a power binds more tightly
    -- than juxtaposition: where the base is in parentheses right after p,
    -- it's p's argument, and the power is of the result
    applyTo lsr r = case r of
      Syn rsr (EBin OpPow base ex)
        | parenthesized rsr base -> (\call ex' -> Syn rsr (EBin OpPow call ex'))
                                      <$> (apply lsr =<< go base) <*> go ex
      _ -> apply lsr =<< go r

    parenthesized rsr base = case (rsr, synAnn base) of
      (SourceRange s _, SourceRange s' _) -> s < s'
      _ -> False

    apply sr = applyFunction tag sr fn

    -- `p'(x)`: the derivative of the function given for p, at x
    derivOf sr order args = case fn of
      Syn _ (EFunRef f) -> pure (Syn sr (EDerivCall f order args))
      Syn _ (ELambda [w] body) -> do
        arg <- splitArgs sr "this function" 1 "" args >>= \case
          (_, [a]) -> pure a
          _ -> throwError (Internal (Advice sr "splitArgs: wrong number of arguments"))
        let t = "[internal] " ++ tag ++ "." ++ w ++ " " ++ position sr
            x = Syn sr (EVar t)
            body' = rename (Map.singleton w t) body
        -- the variable must be at least real to differentiate by it
        pure (Syn sr (ELet t (Syn (synRange arg) (EWiden RealT arg))
                             (iterate (Syn sr . EDiff x) body' !! order)))
      Syn _ (ELambda _ _) ->
        advise sr ("Only a function of one variable can be differentiated with "
                   ++ p ++ replicate order '\'' ++ ".")
      -- (worked out where compositions are: see 'expandCalls', and
      -- "Language.Code.FunctionBlocks")
      Syn _ ECompose{} -> case args of
        [arg] -> pure (Syn sr (EDerivOf fn order arg))
        _ -> throwError (Internal (Advice sr "substituteFunction: a derivative's arguments"))
      _ -> throwError (Internal (Advice sr "substituteFunction: not a function"))
