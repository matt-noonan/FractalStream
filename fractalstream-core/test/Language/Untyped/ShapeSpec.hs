module Language.Untyped.ShapeSpec (spec) where

import Test.Hspec

import Language.Untyped.Shape
import Language.Untyped.Value
import Language.Value.Parser
import Helper.UnionFind

import qualified Data.Map as Map
import Control.Monad.State
import Data.STRef

toValue :: String -> Value
toValue s = case parseUntypedValue s of
  Left e  -> error (show e)
  Right v -> v

infer :: [String] -> [String] -> ([Either ShapeError (ValueWith TypeVar)], [(String, TypeVar)])
infer vars ss = runST $ do
  ctx <- Map.fromList <$> mapM (\v -> (v,) <$> fresh (STShape Unknown)) vars
  ss' <- mapM (inferShape ctx . toValue) ss
  nextTV <- newSTRef 0
  ctx' <- forM ctx (shapeToTS nextTV [])
  let toTS = (`evalStateT` ctx') . toTypeShape nextTV
  (,) <$> mapM toTS ss' <*> pure (Map.toList ctx')

spec :: Spec
spec = do

  describe "when inferring shapes" $ do

    it "can infer the shape of basic operations" $ do
      let (_, ctx1) = infer ["x"] ["x + 2"]
          (_, ctx2) = infer ["x"] ["cos x = x"]
          (_, ctx3) = infer ["x"] ["1 = 2 and x"]
          (_, ctx4) = infer ["x"] ["dark x"]
          (_, ctx5) = infer ["x"] ["x = ((true, 42), dark blue)"]
          (_, ctx6) = infer ["x", "y"] ["x = y", "x = 42"]
          (_, ctx7) = infer ["x"] ["((false, x), red) = ((true, 42), dark blue)"]
      ctx1 `shouldBe` [("x", NumTV 0)]
      ctx2 `shouldBe` [("x", NumTV 0)]
      ctx3 `shouldBe` [("x", BoolTV)]
      ctx4 `shouldBe` [("x", ColorTV)]
      ctx5 `shouldBe` [("x", PairTV (PairTV BoolTV (NumTV 0)) ColorTV)]
      ctx6 `shouldBe` [("x", NumTV 0), ("y", NumTV 1)]
      ctx7 `shouldBe` [("x", NumTV 0)]

    it "can infer erroneous shapes" $ do
      let (_, ctx1) = infer ["x"] ["x + 2 = 1 and x"]
          (_, ctx2) = infer ["x"] ["x"]
          (_, ctx3) = infer ["x"] ["x + y"]
          (_, ctx4) = infer ["x"] ["x = (x, 42)"]
          (_, ctx5) = infer ["x", "y"] ["x = ((x = x, y), z)"]
      ctx1 `shouldBe` [("x", ErrorTV Inconsistent)]
      ctx2 `shouldBe` [("x", ErrorTV Indeterminate)]
      ctx3 `shouldBe` [("x", ErrorTV (Unbound "y"))]
      ctx4 `shouldBe` [("x", PairTV (ErrorTV Infinite) (NumTV 0))]
      ctx5 `shouldBe` [ ("x", PairTV (PairTV BoolTV
                                             (ErrorTV Indeterminate))
                                     (ErrorTV (Unbound "z")))
                      , ("y", ErrorTV Indeterminate)]
