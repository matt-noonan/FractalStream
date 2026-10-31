module Language.Value.DerivativeSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Value.Evaluator

import qualified Language.Value.Parser as P

parseValue :: EnvironmentProxy env
           -> TypeProxy t
           -> String
           -> Either String (Value '(env, t))
parseValue env ty i = withEnvironment env $ withKnownType ty $ first (`P.ppFullError` i) (P.parseValue i)

spec :: Spec
spec = do
  describe "when parsing derivatives" $ do
    let envR = declare @"x" RealType $ endOfDecls
        ctxR x = Bind (Proxy @"x") RealType x $ EmptyContext
        parseR x s = (`evaluate` (ctxR x)) <$> (parseValue envR RealType s)

        envC = declare @"z" ComplexType $ endOfDecls
        ctxC x = Bind (Proxy @"z") ComplexType x $ EmptyContext
        parseC z s = (`evaluate` (ctxC z)) <$> (parseValue envC ComplexType s)

        -- exp and log round slightly differently on different platforms
        -- (e.g. 4.000000000000001 on Windows), so allow for that.
        shouldBeNear actual expected = case actual of
          Right z | magnitude (z - expected) < 1.0e-12 -> pure ()
          _ -> actual `shouldBe` Right expected

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

    it "can differentiate inverse trigonometric functions" $ do
      parseR 0.5 "diff(x, arcsin(x))" `shouldBe` Right (1 / sqrt 0.75)
      parseR 0.5 "diff(x, arccos(x))" `shouldBe` Right (-1 / sqrt 0.75)
      parseR 1 "diff(x, arctan(x))" `shouldBe` Right 0.5
      parseR 1 "diff(x, arcsinh(x))" `shouldBe` Right (1 / sqrt 2)
      parseR 2 "diff(x, arccosh(x))" `shouldBe` Right (1 / sqrt 3)
      parseR 0.5 "diff(x, arctanh(x))" `shouldBe` Right (1 / 0.75)

    it "treats parts that don't depend on the variable as constants" $
      parseR 3 "diff(x, x |2 - 7|)" `shouldBe` Right 5

    it "can differentiate powers with variable exponents" $
      parseR 2 "diff(x, x^x)" `shouldBe` Right (4 * (log 2 + 1))

    it "can apply the chain rule correctly" $ do
      parseR 4 "diff(x, 8 * log(sqrt x))" `shouldBe` Right 1
      parseC 2 "diff(z, exp(z^2) / e^4)" `shouldBeNear` (4 :+ 0)
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
        ,"          ^^^"
        ,""
        ,"The derivative of this function with respect to x is not implemented."])
      parseC 1 "diff(z, im z)" `shouldBe` Left (unlines
        ["  diff(z, im z)"
        ,"          ^^^^"
        ,""
        ,"The derivative of this function with respect to z is not implemented."])

    it "throws errors when first argument is not a variable" $ do
      parseR 0 "diff(2, x)" `shouldBe` Left (unlines
        ["  diff(2, x)"
        ,"       ^"
        ,""
        ,"First argument of the derivative must be a real or complex variable."])
      parseC 1 "diff(z^2, z)" `shouldBe` Left (unlines
        ["  diff(z^2, z)"
        ,"       ^^^"
        ,""
        ,"First argument of the derivative must be a real or complex variable."])