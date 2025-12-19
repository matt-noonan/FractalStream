module Language.Value.DerivativeSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Value.Evaluator

import qualified Data.Map as Map
import qualified Language.Value.Parser as P

parseValue :: EnvironmentProxy env
           -> TypeProxy t
           -> String
           -> Either String (Value '(env, t))
parseValue env ty i = withEnvironment env $ withKnownType ty $ first (`P.ppFullError` i) (P.parseValue Map.empty i)

spec :: Spec
spec = do
  describe "when parsing derivatives" $ do
    let envR = declare @"x" RealType $ endOfDecls
        ctxR x = Bind (Proxy @"x") RealType x $ EmptyContext
        parseR x s = (`evaluate` (ctxR x)) <$> (parseValue envR RealType s)

        envC = declare @"z" ComplexType $ endOfDecls
        ctxC x = Bind (Proxy @"z") ComplexType x $ EmptyContext
        parseC z s = (`evaluate` (ctxC z)) <$> (parseValue envC ComplexType s)

    it "can differentiate constants" $ do
      parseR 2 "diff(x, 42)" `shouldBe` Right 0
      parseR 1 "diff(x, pi)" `shouldBe` Right 0

    it "can differentiate algebraic expressions" $ do
      parseR 3 "diff(x, x)" `shouldBe` Right 1
      parseR 1 "diff(x, 3x + 1)" `shouldBe` Right 3
      parseR 2 "diff(x, x * x)" `shouldBe` Right 4
      parseR 1 "diff(x, x^2)" `shouldBe` Right 2
      parseR 1 "diff(x, 1/x^3)" `shouldBe` Right (-3)

    it "can differentiate transcendental functions" $ do
      parseR pi "diff(x, sin(x))" `shouldBe` Right (-1)
      parseR 0 "diff(x, exp(x))" `shouldBe` Right 1
      parseR 1 "diff(x, log(x))" `shouldBe` Right 1

    it "can apply the chain rule correctly" $ do
      parseR 4 "diff(x, 8 * log(sqrt x))" `shouldBe` Right 1
      parseC 2 "diff(z, exp(z^2) / e^4)" `shouldBe` Right (4 :+ 0)
      parseC 2 "diff(z, log(z^2))" `shouldBe` Right (1.0 :+ 0.0)

    it "throws errors when trying to differentiate non-numeric variables" $ do
      parseR 0 "diff(x, 2x+1 < 1)" `shouldBe` Left (unlines 
        ["  diff(x, 2x+1 < 1)"
        ,"          ^^^^^^^^"
        ,""
        ,"I expected a real number here, but the result of a comparison is a truth value."])

    it "throws errors when trying to differentiate non-differentiable functions" $ do
      parseR 0 "diff(x, |x|)" `shouldBe` Left (unlines 
        ["  diff(x, |x|)"
        ,"  ^^^^^^^^^^^^"
        ,""
        ,"The derivative of this function with respect to x is not implemented."])
      parseC 1 "diff(z, im z)" `shouldBe` Left (unlines 
        ["  diff(z, im z)"
        ,"  ^^^^^^^^^^^^^"
        ,""
        ,"The derivative of this function with respect to z is not implemented."])