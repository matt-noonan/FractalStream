module Language.Code.Simulator
  ( simulate
  , ScalarTypeM
  ) where

import Language.Value hiding (get)
import Language.Code  hiding (get, set)
import Language.Value.Evaluator

import Data.Indexed.Functor

import Control.Monad.State
import GHC.TypeLits
import Fcf (Exp, Eval)

data ScalarTypeM :: * -> (Environment, Type) -> Exp *
type instance Eval (ScalarTypeM s '(env, t)) =
  State (Context ScalarTypeOfBinding env, s) (ScalarType t)

-- | Update a variable in the current environment
update :: forall name t env s
        . KnownSymbol name
       => NameIsPresent name t env
       -> Proxy name
       -> ScalarProxy t
       -> ScalarType t
       -> State (Context ScalarTypeOfBinding env, s) ()
update pf _name t v = withKnownType t (modify' (\(x,y) -> (setBinding pf v x, y)))

-- | Evaluate a value in the current environment
eval :: forall t env s
      . Value env t
     -> State (Context ScalarTypeOfBinding env, s) (ScalarType t)
eval v = do
  ctx <- fst <$> get
  pure (evaluate ctx v)

-- | Run some 'Code' by interpreting it into a state monad.
-- The 's' parameter allows for extra state that may be used
-- by the effects handlers.
simulate :: forall effs env t s
          . Handlers effs (ScalarTypeM s)
         -> Code effs env t
         -> State (Context ScalarTypeOfBinding env, s) (ScalarType t)
simulate handlers = indexedFold @(ScalarTypeM s) @(Fix (CodeF effs Value_)) @(CodeF effs Value_) $ \case
  Let pf name vt v _ body -> recallIsAbsent (absentInTail pf) $ do
    (ctx, s) <- get
    value <- eval v
    let ctx' = Bind name vt value ctx
        (result, (Bind _ _ _ ctx'', s'')) = runState body (ctx', s)
    put (ctx'', s'')
    pure result

  LetBind pf name tv vc _tr body -> recallIsAbsent (absentInTail pf) $ do
    (ctx, s) <- get
    value <- vc
    let ctx' = Bind name tv value ctx
        (result, (Bind _ _ _ ctx'', s'')) = runState body (ctx', s)
    put (ctx'', s'')
    pure result

  Set pf name v -> do
    result <- eval v
    update pf name (typeOfValue v) result

  SetBind pf name tv vc -> do
    result <- vc
    update pf name tv result

  Call _ code -> do
    st <- get
    pure (evalState code st)

  Block _ stmts stmt -> do
    sequence_ stmts
    stmt

  Pure _ v -> eval v

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
      Handle _ handle -> handle (envProxy env) ty eff
