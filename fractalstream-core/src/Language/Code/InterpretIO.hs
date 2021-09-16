{-# language AllowAmbiguousTypes #-}
module Language.Code.InterpretIO
  ( interpretToIO
  , interpretToIO_
  , eval
  , update
  , IORefTypeOfBinding
  , ScalarIORefM
  , ScalarIORefMWith
  ) where

import Language.Value hiding (get)
import Language.Code  hiding (get, set)
import Language.Value.Evaluator

import Data.Indexed.Functor

import Data.IORef
import Control.Monad.State
import GHC.TypeLits
import Fcf (Exp, Eval)

data ScalarIORefM :: (Environment, Type) -> Exp *
type instance Eval (ScalarIORefM '(env, t)) =
  StateT (Context IORefTypeOfBinding env) IO (ScalarType t)

data ScalarIORefMWith :: * -> (Environment, Type) -> Exp *
type instance Eval (ScalarIORefMWith s '(env, t)) =
  StateT (Context IORefTypeOfBinding env, s) IO (ScalarType t)

data IORefTypeOfBinding :: Symbol -> Type -> Exp *
type instance Eval (IORefTypeOfBinding name t) = IORef (ScalarType t)

-- | Evaluate a value in the current environment
eval :: forall t env s
      . Value env t
     -> StateT (Context IORefTypeOfBinding env, s) IO (ScalarType t)
eval v = do
  ctxRef <- fst <$> get
  ctx <- mapContextM (\_ _ -> lift . readIORef) ctxRef
  pure (evaluate ctx v)

-- | Update a variable in the current environment
update :: forall name t env s
        . KnownSymbol name
       => NameIsPresent name t env
       -> Proxy name
       -> ScalarProxy t
       -> ScalarType t
       -> StateT (Context IORefTypeOfBinding env, s) IO ()
update pf _name t v = withKnownType t $ do
  ctx <- fst <$> get
  let valueRef = getBinding ctx pf
  lift (writeIORef valueRef v)

interpretToIO :: forall effs env0 t0
               . Handlers effs ScalarIORefM
              -> Code effs env0 t0
              -> StateT (Context IORefTypeOfBinding env0) IO (ScalarType t0)
interpretToIO handlers = fro (Proxy @env0) (Proxy @t0)
                       . interpretToIO_ @_ @_ @_ @() (mapHandlers to fro handlers)
  where
    to  :: forall env t pxy1 pxy2
         . pxy1 env
        -> pxy2 t
        -> StateT (Context IORefTypeOfBinding env) IO (ScalarType t)
        -> StateT (Context IORefTypeOfBinding env, ()) IO (ScalarType t)
    to _ _ s = do
      (ctx, _) <- get
      (result, ctx') <- lift (runStateT s ctx)
      put (ctx', ())
      pure result

    fro :: forall env t pxy1 pxy2
         . pxy1 env
        -> pxy2 t
        -> StateT (Context IORefTypeOfBinding env, ()) IO (ScalarType t)
        -> StateT (Context IORefTypeOfBinding env) IO (ScalarType t)
    fro _ _ s = do
      ctx <- get
      (result, (ctx', _)) <- lift (runStateT s (ctx, ()))
      put ctx'
      pure result

interpretToIO_ :: forall effs env t s
                . Handlers effs (ScalarIORefMWith s)
               -> Code effs env t
               -> StateT (Context IORefTypeOfBinding env, s) IO (ScalarType t)
interpretToIO_ handlers =
  indexedFold @(ScalarIORefMWith s) @(Fix (CodeF effs)) @(CodeF effs) $ \case
    Let pf name v _ body -> recallIsAbsent (absentInTail pf) $ do
      (ctxRef, s) <- get
      value <- eval v
      valueRef <- lift (newIORef value)
      let ctxRef' = Bind name (typeOfValue v) valueRef ctxRef
      lift (evalStateT body (ctxRef', s))

    LetBind pf name tv vc _ body -> recallIsAbsent (absentInTail pf) $ do
      (ctxRef, _) <- get
      value <- vc
      valueRef <- lift (newIORef value)
      let ctxRef' = Bind name tv valueRef ctxRef
      s <- snd <$> get
      lift (evalStateT body (ctxRef', s))

    Set pf name v -> do
      value <- eval v
      update pf name (typeOfValue v) value

    SetBind pf name tv vc -> do
      value <- vc
      update pf name tv value

    Call _ code -> do
      (ctx, s) <- get
      lift $ do
        ctxCopy <- forContextM ctx (\_ _ ref -> readIORef ref >>= newIORef)
        evalStateT code (ctxCopy, s)

    Block _ stmts stmt -> do
      sequence_ stmts
      stmt

    Pure v -> eval v

    NoOp -> pure ()

    DoWhile body -> loop
      where loop = do
              continue <- body
              if continue then loop else pure ()

    IfThenElse _ test yes no -> do
      tf <- eval test
      if tf then yes else no

    Effect effectType env ty eff ->
      case getHandler effectType handlers of
        Handle _ handle -> handle (envProxy env) ty eff
