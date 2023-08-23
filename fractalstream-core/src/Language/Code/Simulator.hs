module Language.Code.Simulator
  ( simulate
  , HaskellTypeM
  ) where

import Language.Value hiding (get)
import Language.Code  hiding (get, set)
import Language.Value.Evaluator

import Data.Indexed.Functor

import Control.Monad.State
import GHC.TypeLits
import Fcf (Exp, Eval, Pure1)
import Data.Kind

data HaskellTypeM :: Type -> (Environment, FSType) -> Exp Type
type instance Eval (HaskellTypeM s '(env, t)) =
  State (Context HaskellTypeOfBinding env, s) (HaskellType t)

-- | Update a variable in the current environment
update :: forall name t env s
        . KnownSymbol name
       => NameIsPresent name t env
       -> Proxy name
       -> TypeProxy t
       -> HaskellType t
       -> State (Context HaskellTypeOfBinding env, s) ()
update pf _name t v = withKnownType t (modify' (\(x,y) -> (setBinding pf v x, y)))

-- | Evaluate a value in the current environment
eval :: forall t env s
      . Value '(env, t)
     -> State (Context HaskellTypeOfBinding env, s) (HaskellType t)
eval v = do
  ctx <- fst <$> get
  pure (evaluate v ctx)

-- | Run some 'Code' by interpreting it into a state monad.
-- The 's' parameter allows for extra state that may be used
-- by the effects handlers.
simulate :: forall effs env t s
          . Handlers effs (HaskellTypeM s)
         -> Code effs env t
         -> State (Context HaskellTypeOfBinding env, s) (HaskellType t)
simulate handlers = indexedFold @(HaskellTypeM s) @(Fix (CodeF effs (Pure1 Value))) @(CodeF effs (Pure1 Value)) $ \case
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
