module Language.Code.Simulator
  ( simulate
  , eval
  , HaskellTypeM
  ) where

import FractalStream.Prelude

import Language.Value
import Language.Code
import Language.Value.Evaluator
import Language.Draw

import Data.Indexed.Functor

data HaskellTypeM :: Type -> Environment -> Exp Type
type instance Eval (HaskellTypeM s env) =
  State (Context HaskellValue env, s) ()

-- | Update a variable in the current environment
update :: forall name t env s
        . KnownSymbol name
       => NameIsPresent name t env
       -> Proxy name
       -> TypeProxy t
       -> HaskellType t
       -> State (Context HaskellValue env, s) ()
update pf _name t v = withKnownType t (modify' (\(x,y) -> (setBinding pf v x, y)))

-- | Evaluate a value in the current environment
eval :: forall t env s
      . Value '(env, t)
     -> State (Context HaskellValue env, s) (HaskellType t)
eval v = do
  ctx <- fst <$> get
  pure (evaluate v ctx)

-- | Run some 'Code' by interpreting it into a state monad.
-- The 's' parameter allows for extra state that may be used
-- by the effects handlers.
simulate :: forall env s
          . DrawHandler (HaskellTypeM s)
         -> Code env
         -> State (Context HaskellValue env, s) ()
simulate draw = indexedFold @(HaskellTypeM s) $ \case

  Let pf name vc body -> recallIsAbsent (absentInTail pf) $ do
    (ctx, s) <- get
    value <- eval vc
    let ctx' = Bind name (typeOfValue vc) value ctx
        (result, (Bind _ _ _ ctx'', s'')) = runState body (ctx', s)
    put (ctx'', s'')
    pure result

  Set pf name vc -> do
    result <- eval vc
    update pf name (typeOfValue vc) result

  Block stmts -> sequence_ stmts

  NoOp -> pure ()

  DoWhile cond body -> loop
    where loop = do
            body
            tf <- eval cond
            if tf then loop else pure ()

  IfThenElse test t f -> do
    tf <- eval test
    if tf then t else f

  DrawCommand d -> runDrawHandler draw d

  Lookup pfList listName listTy@(ListType itemTy) itemName pfNoItem _ env predicate action fallback ->
        recallIsAbsent pfNoItem $ do
        let go = \case
              [] -> fromMaybe (pure ()) fallback
              (item : items) -> do
                (ctx, s) <- get
                let ctx' = Bind itemName itemTy item ctx
                    (matches, (Bind _ _ _ ctx'', s')) = runState (eval predicate) (ctx', s)
                put (ctx'', s')
                case matches of
                  True -> lift (evalStateT action (ctx', s))
                  False -> go items
        go =<< eval (withEnvironment env $ Var listName listTy pfList)

  ForEach pfList listName listTy@(ListType itemTy) itemName pfNoItem env _ body ->
    recallIsAbsent pfNoItem $ do
    let go = \case
          [] -> pure ()
          (item : items) -> do
            (ctx, s) <- get
            let ctx' = Bind itemName itemTy item ctx
                (Bind _ _ _ ctx'', s') = execState body (ctx', s)
            put (ctx'', s')
            go items
    go =<< eval (withEnvironment env $ Var listName listTy pfList)
