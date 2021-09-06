{-# options_ghc -Wno-type-defaults #-}

module Lang.Expr.Typing
  ( TypedExpr
  , typeExpr
  , Context(..)
  , emptyContext
  , TypeError(..)
  , TCStateT
  , runTC
  , runTCStateT
  , fresh
  , getType
  , getTypeRep
  , setType
  , setType'
  , unify
  , unifyVars
  , unifyAll
  , unifiedWith
  , mergeTypes
  , inferType
  , infer
  , checkType
  ) where

import           Lang.Expr
import           Lang.Expr.Print        ()

import           Data.List              (intercalate)
import           Data.Map               (Map)
import qualified Data.Map               as Map

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State

type TypedExpr = AExpr Type

-- | Infer types for each subexpression and annotate each AST node
--   with its inferred type.
typeExpr :: Monad m => Expr -> TCStateT m TypedExpr
typeExpr = annotateWithM inferType

data Context = Context
  { bindings    :: Map Symbol Type
  , tempSymbols :: [Symbol]
  } deriving (Eq, Ord)

instance Show Context where
  show ctx = "{" ++ intercalate "," bs ++ " }"
    where bs = map (\(s,t) -> " " ++ show s ++ ":" ++ show t) $ Map.toList (bindings ctx)

emptyContext :: Context
emptyContext = Context
  { bindings = Map.empty
  , tempSymbols = zipWith (:) (repeat '#') (map show [0..])
  }

data TypeError
  = CouldNotUnify Symbol Type Type
  | CouldNotUnifyTypes Type Type
  | AssertionFailure String
  | NoFreshVariablesLeft
  | NotAFunction Type
  deriving (Eq, Ord, Show)

-- | State monad for type checker; threads a TypeContext
--   state or an error of type TypeError.
type TCStateT m = StateT Context (ExceptT TypeError m)
type TCState = TCStateT Identity

runTC :: Context -> TCState a -> Either TypeError (a, Context)
runTC ctx f = (runIdentity . runExceptT . runStateT f) ctx

runTCStateT :: Monad m => Context -> TCStateT m a -> m (Either TypeError (a, Context))
runTCStateT ctx f = (runExceptT . runStateT f) ctx

fresh :: Monad m => TCStateT m Symbol
fresh = do
  ctx <- get
  let freshes = tempSymbols ctx
  case freshes of
    []     -> throwError NoFreshVariablesLeft
    (f:fs) -> put (ctx { tempSymbols = fs }) >> return f

-- | Lookup the type of a symbol;
getType :: Monad m => Symbol -> TCStateT m Type

getType s = do
  ctx <- get
  let t = Map.lookup s (bindings ctx)
  case t of
    Just t' -> getTypeRep t'
    Nothing -> do
      let t' = Underconstrained s
      put $ ctx { bindings = Map.insert s t' (bindings ctx) }
      return t'

getTypeRep :: Monad m => Type -> TCStateT m Type
getTypeRep (TypeOf s) = getType s
getTypeRep t          = return t

setType :: Monad m => Symbol -> Type -> TCStateT m Type
setType s t = do
  t' <- getType s
  case t' of
    Underconstrained s' -> do
      ctx <- get
      put $ ctx { bindings = Map.insert s' t (bindings ctx) }
      return t
    t'' -> if t == t''
           then return t
           else throwError $ CouldNotUnify s t t''

setType' :: Monad m => Symbol -> Type -> TCStateT m ()
setType' s t = setType s t >>= \_ -> return ()

unifyVars :: Monad m => Symbol -> Symbol -> TCStateT m ()
unifyVars s s' = do
  t  <- getType s
  t' <- getType s'
  _  <- unify t t'
  return ()

unify :: Monad m => Type -> Type -> TCStateT m ()
unify t t' = t `unifiedWith` t' >>= \_ -> return ()

unifiedWith :: Monad m => Type -> Type -> TCStateT m Type
unifiedWith t t' = do
  let getRep (Underconstrained x) = Just x
      getRep _                    = Nothing

  case (getRep t, getRep t') of
    (Nothing, Nothing) -> case mergeTypes t t' of
                            Just t'' -> return t''
                            Nothing  -> throwError $ CouldNotUnifyTypes t t'
    (Just r,  Nothing)  -> setType r  t'
    (Nothing, Just r')  -> setType r' t
    (Just _, Just r')   -> setType r' t

mergeTypes :: Type -> Type -> Maybe Type
mergeTypes t t'
  | t == Complex_T && t' == Real_T   = Just Complex_T
  | t == Real_T && t' == Complex_T   = Just Complex_T
  | t == t'                          = Just t
  | otherwise                        = Nothing

unifyAll :: Monad m => [Type] -> TCStateT m Type
unifyAll []     = throwError $ AssertionFailure "no types to unify"
unifyAll (t:ts) = foldM unifiedWith t ts

inferType :: Monad m => Expr -> TCStateT m Type
inferType = foldExprM infer

infer :: Monad m => ExprF Type -> TCStateT m Type
infer expr = case expr of
   Let s x a                -> getType s >>= unify x >> return a

   Var s                    -> getType s

   Apply (Func_T dom cod) x -> unify x dom >> return cod
   Apply f _                -> throwError $ NotAFunction f

   Lambda s e               -> getType s >>= \t -> return $ Func_T t e

   Const _                  -> return Real_T
   I                        -> return Complex_T
   Add xs                   -> unifyAll xs
   Embed _                  -> return Complex_T
   Sub x y                  -> x `unifiedWith` y
   Mul xs                   -> unifyAll xs
   Div x y                  -> x `unifiedWith` y
   Pow x n                  -> x `unifiedWith` n
   Neg x                    -> return x
   Conj x                   -> return x
   RealPart x               -> return x
   ImagPart x               -> return x
   Norm _                   -> return Real_T
   Norm2 _                  -> return Real_T
   Diff _ x                 -> return x
   Case _ _                 -> error "TODO"
   Builtin f                -> return $ builtinFuncType f
   Pair _ _                 -> error "TODO"
   Transpose _              -> error "TODO"
   Row _ _                  -> error "TODO"
   Col _ _                  -> error "TODO"

checkType :: Monad m => Context -> Expr -> Type -> m Bool
checkType ctx e t = do
  result <- runTCStateT ctx $ inferType e
  return $ case result of
    Right (t',_) -> t == t'
    Left _       -> False
