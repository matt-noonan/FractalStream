{-# language QuasiQuotes #-}

module Language.Effect.ListSpec (spec) where

import Test.Hspec

import Language.Type
import Language.Environment
import Language.Parser
import Language.Value hiding (get)
import Language.Value.Evaluator
import Language.Code.Parser
import Language.Code.Simulator
import Language.Effect.List
import Control.Monad.State
import Language.Effect

import Text.RawString.QQ

runWithXY :: forall t xt yt
           . ScalarProxy t
          -> Scalar xt
          -> Scalar yt
          -> [Double]
          -> String
          -> Either (Int, BadParse) (ScalarType t, [Double])
runWithXY t (Scalar xt x) (Scalar yt y) list0 input = withKnownType xt $ withKnownType yt $
  let ctx = Bind (Proxy @"x") xt x
          $ Bind (Proxy @"y") yt y
          $ EmptyContext
      getList v = do
        (_, xs) <- get
        pure (v, xs)
  in fmap ((`evalState` (ctx, list0)) . (>>= getList) . simulate (Handler listHandler NoHandler))
   $ parseCode (EP (ParseEff listEffectParser NoEffs)) (envProxy Proxy) t input

-- | Evaluate a value in the current environment
eval :: forall t env s
      . Value env t
     -> State (Context ScalarTypeOfBinding env, s) (ScalarType t)
eval v = do
  ctx <- fst <$> get
  pure (evaluate ctx v)

-- | Mix in list manipulation effects
listHandler :: EffectHandler (List "test" 'RealT) (ScalarTypeM [Double])
listHandler = Handle Proxy handle
  where
    handle :: forall env t
            . EnvironmentProxy env
           -> ScalarProxy t
           -> List "test" 'RealT (ScalarTypeM [Double]) '(env, t)
           -> State (Context ScalarTypeOfBinding env, [Double]) (ScalarType t)
    handle _ _ = \case

      Insert _ _ _ v -> do
        x <- eval v
        modify' (\(ctx, s) -> (ctx, x : s))

      Lookup _ _ _ pf _ _ test match miss -> recallIsAbsent pf $ do
        let test' item = do
              (ctx, _) <- get
              let ctx' = Bind Proxy RealProxy item ctx
              pure (evaluate ctx' test)

            go [] = case miss of
                VNothing     -> pure ()
                VJust action -> action
            go (item:items) = test' item >>= \case
              False -> go items
              True  -> do -- evaluate continuation with "item" bound
                (ctx, s) <- get
                let ctx' = Bind Proxy RealProxy item ctx
                    (result, (Bind _ _ _ ctx'', s'')) = runState match (ctx', s)
                put (ctx'', s'')
                pure result
        (_, items) <- get
        go items

      ClearList _ _ _ -> modify' (\(ctx, _) -> (ctx, []))

      Remove _ _ _ pf _ test -> recallIsAbsent pf $ do
        (ctx, items) <- get
        let reject item = evaluate (Bind Proxy RealProxy item ctx) test
        put (ctx, filter (not . reject) items)

      ForEach _ _ _ pf _ _ body -> recallIsAbsent pf $ do
        items <- snd <$> get
        forM_ items $ \item -> do
          (ctx, s) <- get
          let ctx' = Bind Proxy RealProxy item ctx
              (Bind _ _ _ ctx'', s'') = execState body (ctx', s)
          put (ctx'', s'')

spec :: Spec
spec = do

  describe "when using list effects in code" $ do

    it "can parse embedded list effects" $ do

      let p1 = [r|
set x to 0
for each item in test do
  set x to x + item
set y to x
y|]
      runWithXY RealProxy (Scalar RealProxy 5) (Scalar RealProxy 7) [1,2,3,4,5] p1
        `shouldBe` Right (15, [1,2,3,4,5])

      let p2 = [r|
set x to 3
remove each item matching item <= x from test|]
      runWithXY VoidProxy (Scalar RealProxy 5) (Scalar RealProxy 7) [1,2,3,4,5] p2
        `shouldBe` Right ((), [4,5])

      let p3 = [r|
set x to 3
if |x - y| < 2 then
  remove all items from test|]

      runWithXY VoidProxy (Scalar RealProxy 1) (Scalar RealProxy 10) [1,2,3,4,5] p3
        `shouldBe` Right ((), [1,2,3,4,5])
      runWithXY VoidProxy (Scalar RealProxy 1) (Scalar RealProxy 4) [1,2,3,4,5] p3
        `shouldBe` Right ((), [])


{-
with first ac matching |z - proj_1 ac| < minRadius in autocolors
  output proj_2 ac to color
else
  init newColor <- random
  insert (z, newColor) at end of autocolors
  output newColor to color
-}
