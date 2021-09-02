module Language.Value.EvaluatorSpec (spec) where

import Test.Hspec
import Language.Value
import Language.Value.Evaluator
import Data.Indexed.Functor

spec :: Spec
spec = do

  describe "The 'evaluate' function" $ do

    it "computes real values from an empty context" $ do
      let v1, v2, v3 :: Value '[] 'RealT
          v1 = cos(pi) + abs(10 - 20)
          v2 = (10 * 10) / 20 - 30
          v3 = exp(2 * log(pi))
          -- Double check that the expression for v1 didn't
          -- get computed by Haskell *before* it was
          -- turned into a Value.
          v1_outermost_add = case v1 of
            Fix (AddF _ _) -> True
            _ -> False
      v1_outermost_add `shouldBe` True
      evaluate EmptyContext v1 `shouldBe` 9
      evaluate EmptyContext v2 `shouldBe` (-25)
      evaluate EmptyContext v3 `shouldBe` (pi * pi)

    it "computes integer values from an empty context" $ do
      let v1, v2, v3, v4 :: Value '[] 'IntegerT
          v1 = abs(20 - 30)
          v2 = Fix (DivI (10 * 10) 20) - 30
          v3 = Fix (DivI 20 7)
          v4 = Fix (ModI 20 7)
      evaluate EmptyContext v1 `shouldBe` 10
      evaluate EmptyContext v2 `shouldBe` (-25)
      evaluate EmptyContext v3 `shouldBe` 2
      evaluate EmptyContext v4 `shouldBe` 6

    it "computes boolean values from an empty context" $ do
      let yes, no, v1, v2, v3, v4 :: Value '[] 'BooleanT
          yes = Fix (Const (Scalar BooleanProxy True ))
          no  = Fix (Const (Scalar BooleanProxy False))
          v1 = Fix (Eql RealProxy
                      (cos(pi) + abs(10 - 20))
                      (3 * 3))
          v2 = Fix (Eql BooleanProxy
                      (Fix (Or yes no))
                      (Fix (And yes no)))
          v3 = Fix (NEq IntegerProxy
                      (Fix (DivI (10 * 10) 20) - 30)
                      (50 - 75))
          v4 = Fix (Eql (PairProxy IntegerProxy IntegerProxy)
                      (Fix (PairV (PairProxy IntegerProxy IntegerProxy)
                                  (Fix (DivI 20 7))
                                  (Fix (ModI 20 7))))
                      (Fix (PairV (PairProxy IntegerProxy IntegerProxy)
                                  2 6)))
      evaluate EmptyContext v1 `shouldBe` True
      evaluate EmptyContext v2 `shouldBe` False
      evaluate EmptyContext v3 `shouldBe` False
      evaluate EmptyContext v4 `shouldBe` True
