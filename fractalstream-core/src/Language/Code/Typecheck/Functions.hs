-- | User-defined functions (@let f(x, y) = e@), and their expansion:
-- each call is replaced by local bindings of the (freshly renamed)
-- parameters around a copy of the body, so that a function is effectively
-- typechecked anew at each call, at the argument types used there.
module Language.Code.Typecheck.Functions
  ( Funs
  , FunDef(..)
  , defineFunction
  , expandCalls
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
  forM_ (Map.toList (calledFunctions body)) $ \(g, gsr) ->
    unless (Map.member g funs) $ throwError (MissingFunction gsr g)
  pure (Map.insert f (FunDef params body funs) funs)

-- | Replace each call to a user-defined function with local bindings of
-- its (freshly renamed) parameters around a copy of its body, and each
-- derivative call @f'(a)@ with @diff(t, f(t))@ where @t@ is a fresh
-- variable bound to @a@.
expandCalls :: Funs -> Expr -> TC Expr
expandCalls funs0 e0 = evalStateT (go funs0 e0) (0 :: Int)
  where
    go funs (Syn sr node) = case node of
      ECall f args -> do
        (params, body, bodyFuns) <- lookupFun sr f funs
        args' <- traverse (go funs) =<< lift (splitArgs sr f (length params) args)
        names <- freshNames f params
        body' <- go bodyFuns (rename (Map.fromList (zip params names)) body)
        pure (foldr (\(n, a) b -> Syn sr (ELet n a b)) body' (zip names args'))

      EDerivCall f order args -> do
        (params, body, bodyFuns) <- lookupFun sr f funs
        let f' = f ++ replicate order '\''
        param <- case params of
          [p] -> pure p
          p : _ -> advise sr ("Only a function of one variable can be differentiated with "
                              ++ f' ++ ". To differentiate " ++ f
                              ++ " with respect to one of its arguments, use diff instead, like "
                              ++ "diff(" ++ p ++ ", " ++ f
                              ++ "(" ++ intercalate ", " params ++ ")).")
          [] -> throwError (Internal (Advice sr (f ++ " has no parameters")))
        arg <- lift (splitArgs sr f 1 args) >>= \case
          [a] -> go funs a
          _ -> throwError (Internal (Advice sr "splitArgs: wrong number of arguments"))
        t <- freshName f' param
        let x = Syn sr (EVar t)
        body' <- go bodyFuns (rename (Map.singleton param t) body)
        -- the variable must be at least real to differentiate by it
        pure (Syn sr (ELet t (Syn (synRange arg) (EWiden RealT arg))
                             (iterate (Syn sr . EDiff x) body' !! order)))

      _ -> Syn sr <$> traverse (go funs) node

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
-- split the tuple according to the number of parameters.
splitArgs :: SourceRange -> String -> Int -> [Expr] -> TC [Expr]
splitArgs sr f n = \case
  [arg] -> go n arg
  args | length args == n -> pure args
       | otherwise -> wrongCount
  where
    go 1 arg = pure [arg]
    go k (Syn _ (EPair x rest)) = (x :) <$> go (k - 1) rest
    go _ _ = wrongCount
    wrongCount = advise sr (f ++ " takes " ++ show n ++ " argument" ++ (if n == 1 then "" else "s") ++ ".")

-- | Rename free variables.
rename :: Map String String -> Expr -> Expr
rename m (Syn sr node) = Syn sr $ case node of
  EVar n -> EVar (Map.findWithDefault n n m)
  ERemove lst n p -> ERemove (rename m lst) n (rename (Map.delete n m) p)
  EFind lst n p d -> EFind (rename m lst) n (rename (Map.delete n m) p) (rename m d)
  ETransform lst n f -> ETransform (rename m lst) n (rename (Map.delete n m) f)
  ELet n v body -> ELet n (rename m v) (rename (Map.delete n m) body)
  _ -> fmap (rename m) node

-- | Free variables, with the location of one use of each.
freeVars :: Expr -> Map String SourceRange
freeVars (Syn sr node) = case node of
  EVar n -> Map.singleton n sr
  ERemove lst n p -> freeVars lst <> Map.delete n (freeVars p)
  EFind lst n p d -> freeVars lst <> Map.delete n (freeVars p) <> freeVars d
  ETransform lst n f -> freeVars lst <> Map.delete n (freeVars f)
  ELet n v body -> freeVars v <> Map.delete n (freeVars body)
  _ -> foldMap freeVars node

calledFunctions :: Expr -> Map String SourceRange
calledFunctions (Syn sr node) = case node of
  ECall f args -> Map.singleton f sr <> foldMap calledFunctions args
  EDerivCall f _ args -> Map.singleton f sr <> foldMap calledFunctions args
  _ -> foldMap calledFunctions node

