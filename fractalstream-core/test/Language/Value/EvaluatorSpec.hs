module Language.Value.EvaluatorSpec (spec) where

import Test.Hspec
import Language.Value
import Language.Value.Evaluator
import Language.Value.Euclidean
import Data.Int (Int32)
import Control.Monad (forM_)

spec :: Spec
spec = do

  describe "The 'evaluate' function" $ do

    it "computes real values from an empty context" $ do
      let v1, v2, v3 :: Value '( '[], 'RealT)
          v1 = cos(pi) + abs(10 - 20)
          v2 = (10 * 10) / 20 - 30
          v3 = exp(2 * log(pi))
          -- Double check that the expression for v1 didn't
          -- get computed by Haskell *before* it was
          -- turned into a Value.
          v1_outermost_add = case v1 of
            AddF _ _ -> True
            _ -> False
      v1_outermost_add `shouldBe` True
      evaluate v1 EmptyContext `shouldBe` 9
      evaluate v2 EmptyContext `shouldBe` (-25)
      evaluate v3 EmptyContext `shouldBe` (pi * pi)

    it "computes integer values from an empty context" $ do
      let v1, v2, v3, v4 :: Value '( '[], 'IntegerT)
          v1 = abs(20 - 30)
          v2 = DivI (10 * 10) 20 - 30
          v3 = DivI 20 7
          v4 = ModI 20 7
      evaluate v1 EmptyContext `shouldBe` 10
      evaluate v2 EmptyContext `shouldBe` (-25)
      evaluate v3 EmptyContext `shouldBe` 2
      evaluate v4 EmptyContext `shouldBe` 6

    it "computes boolean values from an empty context" $ do
      let yes, no, v1, v2, v3, v4 :: Value '( '[], 'BooleanT)
          yes = Const (Scalar BooleanType True )
          no  = Const (Scalar BooleanType False)
          v1 = Eql RealType
                   (cos(pi) + abs(10 - 20))
                   (3 * 3)
          v2 = Eql BooleanType
                   (Or yes no)
                   (And yes no)
          v3 = NEq IntegerType
                   (DivI (10 * 10) 20 - 30)
                   (50 - 75)
          v4 = Eql (PairType IntegerType IntegerType)
                   (PairV (PairType IntegerType IntegerType)
                         (DivI 20 7)
                         (ModI 20 7))
                   (PairV (PairType IntegerType IntegerType)
                     2 6)
      evaluate v1 EmptyContext `shouldBe` True
      evaluate v2 EmptyContext `shouldBe` False
      evaluate v3 EmptyContext `shouldBe` False
      evaluate v4 EmptyContext `shouldBe` True

  describe "Euclidean division (// and mod)" $ do

    it "gives the remainder in [0, |b|), with a = b (a // b) + mod(a, b)" $ do
      let cases :: [(Integer, Integer, Integer, Integer)]
          cases = [ (7, 3, 2, 1), (-7, 3, -3, 2), (7, -3, -2, 1), (-7, -3, 3, 2)
                  , (6, 3, 2, 0), (-6, 3, -2, 0), (6, -3, -2, 0), (-6, -3, 2, 0)
                  , (0, 5, 0, 0), (0, -5, 0, 0), (5, -1, -5, 0), (-5, 1, -5, 0)
                  , (7, 0, 0, 0), (-7, 0, 0, 0), (0, 0, 0, 0) ]
      forM_ cases $ \(a, b, q, r) -> do
        (a, b, euclidDiv a b) `shouldBe` (a, b, q)
        (a, b, euclidMod a b) `shouldBe` (a, b, r)
      forM_ [-20 .. 20 :: Integer] $ \a -> forM_ ([-7 .. -1] ++ [1 .. 7]) $ \b -> do
        let r = euclidMod a b
        (a, b, r >= 0 && r < abs b, b * euclidDiv a b + r) `shouldBe` (a, b, True, a)

    it "wraps the smallest integer divided by -1, and doesn't overflow" $ do
      let lo = minBound :: Int32
      euclidDiv lo (-1) `shouldBe` lo
      euclidMod lo (-1) `shouldBe` 0
      euclidMod (-1) lo `shouldBe` maxBound
      euclidDiv (-1) lo `shouldBe` 1
      euclidMod lo 3 `shouldBe` 1
      euclidMod lo lo `shouldBe` 0

    it "gives a real remainder in [0, |b|)" $ do
      let cases = [ (7.5, 2, 1.5), (-7.5, 2, 0.5), (7.5, -2, 1.5), (-7.5, -2, 0.5)
                  , (-1, -3, 2), (-1, 3, 2), (1, -3, 1), (3, 3, 0), (-3, 3, 0)
                  , (-1e-20, 3, 0), (-1e-20, -3, 0), (2.5, 0, 0), (0, 1, 0) ]
      forM_ cases $ \(a, b, r) -> (a, b, euclidModReal a b) `shouldBe` (a, b, r :: Double)
      isNaN (euclidModReal (1 / 0) 2) `shouldBe` True
      isNaN (euclidModReal (0 / 0) 2) `shouldBe` True

    it "is what the evaluator computes" $ do
      let i :: Integer -> Value '( '[], 'IntegerT)
          i = fromInteger
      evaluate (ModI (i (-7)) 3) EmptyContext `shouldBe` 2
      evaluate (ModI 7 (i (-3))) EmptyContext `shouldBe` 1
      evaluate (DivI (i (-7)) 3) EmptyContext `shouldBe` (-3)
      evaluate (DivI 7 (i (-3))) EmptyContext `shouldBe` (-2)
      evaluate (DivI 7 0) EmptyContext `shouldBe` 0
      evaluate (ModI 7 0) EmptyContext `shouldBe` 0
      let x :: Double -> Value '( '[], 'RealT)
          x = realToFrac
      evaluate (ModF (x (-1)) (x (-3))) EmptyContext `shouldBe` 2
      evaluate (ModF (x (-1e-20)) 3) EmptyContext `shouldBe` 0
      evaluate (ModF 2.5 0) EmptyContext `shouldBe` 0
