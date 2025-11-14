{-# language AllowAmbiguousTypes, UndecidableInstances #-}
{-# language QuasiQuotes #-}

module Language.Value.DerivativeSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Value.Evaluator
import Language.Value.Derivative

import qualified Data.Map as Map
import qualified Language.Value.Parser as P

parseValue :: EnvironmentProxy env
           -> TypeProxy t
           -> String
           -> Either String (Value '(env, t))
parseValue env ty i = withEnvironment env $ withKnownType ty $
  first (`P.ppFullError` i) (P.parseValue Map.empty i)

spec :: Spec
spec = do
  describe "derivative" $ do
    let env = declare @"x" RealType $ endOfDecls
        ctx x = Bind (Proxy @"x") RealType x $ EmptyContext
        parse s = parseValue env RealType s
        diff x s = (`evaluate` ctx x) <$> derivative "x" <$> parse s

    it "can differentiate constants" $ do
      (diff 2 "42") `shouldBe` Right 0
      (diff 1 "pi") `shouldBe` Right 0

    it "can differentiate algebraic expressions" $ do
      (diff 3 "x") `shouldBe` Right 1
      (diff 1 "3x + 1") `shouldBe` Right 3
      (diff 2 "x * x") `shouldBe` Right 4
      (diff 1 "x^2") `shouldBe` Right 2
      (diff 1 "1/x") `shouldBe` Right (-1)

    it "can differentiate transcendental functions" $ do
      (diff pi "sin(x)") `shouldBe` Right (-1)
      (diff 0 "exp(x)") `shouldBe` Right 1
      (diff 1 "log(x)") `shouldBe` Right 1