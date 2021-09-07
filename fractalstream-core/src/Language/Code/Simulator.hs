module Language.Code.Simulator
  ( simulate
  ) where

import Language.Value hiding (get)
import Language.Code  hiding (get, set)
import Language.Value.Evaluator

import Data.Indexed.Functor

import Control.Monad.State
import GHC.TypeLits
import Fcf (Exp, Eval)

data ScalarTypeM :: (Environment, Type) -> Exp *
type instance Eval (ScalarTypeM '(env, t)) =
  State (Context ScalarTypeOfBinding env) (ScalarType t)

type family Env (et :: (Environment, Type)) where Env '(env, t) = env
type family Ty  (et :: (Environment, Type)) where Ty  '(env, t) = t

-- | Update a variable in the current environment
update :: forall name t env
        . KnownSymbol name
       => NameIsPresent name t env
       -> Proxy name
       -> ScalarProxy t
       -> ScalarType t
       -> State (Context ScalarTypeOfBinding env) ()
update pf _name t v = withKnownType t (modify' (setBinding pf v))

-- | Evaluate a value in the current environment
eval :: forall t env
      . Value env t
     -> State (Context ScalarTypeOfBinding env) (ScalarType t)
eval v = do
  ctx <- get
  pure (evaluate ctx v)

data StateWithEnv :: Environment -> Type -> Exp *
type instance Eval (StateWithEnv env t) =
  State (Context ScalarTypeOfBinding env) (ScalarType t)

-- | Run some effect-free 'Code' by interpreting it into a state monad.
simulate :: forall effs env t
          . Handlers effs StateWithEnv
         -> Code effs env t
         -> State (Context ScalarTypeOfBinding env) (ScalarType t)
simulate handlers = indexedFold @ScalarTypeM @(Fix (CodeF effs)) @(CodeF effs) $ \case
  Let _pf name v _ body -> do
    ctx <- get
    value <- eval v
    let ctx' = Bind name (typeOfValue v) value ctx
        (result, Bind _ _ _ ctx'') = runState body ctx'
    put ctx''
    pure result

  Set pf name v -> do
    result <- eval v
    update pf name (typeOfValue v) result

  Call _ code -> do
    ctx <- get
    pure (evalState code ctx)

  Block _ stmts stmt -> do
    sequence_ stmts
    stmt

  Pure v -> eval v

  NoOp -> pure ()

  DoWhile body -> loop
    where loop = do
            continue <- body
            if continue then loop else pure ()

  IfThenElse _ test t f -> do
    tf <- eval test
    if tf then t else f

  Effect effectType env ty eff ->
    case getHandler effectType handlers of
      Handle _ handle -> handle env ty eff
