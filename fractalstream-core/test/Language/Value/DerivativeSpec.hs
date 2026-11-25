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
      parseR 2 "diff(42, x)" `shouldBe` Right 0
      parseR 1 "diff(pi, x)" `shouldBe` Right 0

    it "can differentiate algebraic expressions" $ do
      parseR 3 "diff(x, x)" `shouldBe` Right 1
      parseR 1 "diff(3x + 1, x)" `shouldBe` Right 3
      parseR 2 "diff(x * x, x)" `shouldBe` Right 4
      parseR 1 "diff(x^2, x)" `shouldBe` Right 2
      parseR 1 "diff(1/x^3, x)" `shouldBe` Right (-3)

    it "can differentiate transcendental functions" $ do
      parseR pi "diff(sin(x), x)" `shouldBe` Right (-1)
      parseR 0 "diff(exp(x), x)" `shouldBe` Right 1
      parseR 1 "diff(log(x), x)" `shouldBe` Right 1

    it "can differentiate inverse trigonometric functions" $ do
      parseR 0.5 "diff(arcsin(x), x)" `shouldBe` Right (1 / sqrt 0.75)
      parseR 0.5 "diff(arccos(x), x)" `shouldBe` Right (-1 / sqrt 0.75)
      parseR 1 "diff(arctan(x), x)" `shouldBe` Right 0.5
      parseR 1 "diff(arcsinh(x), x)" `shouldBe` Right (1 / sqrt 2)
      parseR 2 "diff(arccosh(x), x)" `shouldBe` Right (1 / sqrt 3)
      parseR 0.5 "diff(arctanh(x), x)" `shouldBe` Right (1 / 0.75)

    it "treats parts that don't depend on the variable as constants" $
      parseR 3 "diff(x |2 - 7|, x)" `shouldBe` Right 5

    it "can differentiate powers with variable exponents" $
      parseR 2 "diff(x^x, x)" `shouldBe` Right (4 * (log 2 + 1))

    it "can apply the chain rule correctly" $ do
      parseR 4 "diff(8 * log(sqrt x), x)" `shouldBe` Right 1
      parseC 2 "diff(exp(z^2) / e^4, z)" `shouldBeNear` (4 :+ 0)
      parseC 2 "diff(log(z^2), z)" `shouldBe` Right (1.0 :+ 0.0)

    it "throws errors when trying to differentiate non-numeric variables" $ do
      parseR 0 "diff(2x+1 < 1, x)" `shouldBe` Left (unlines
        ["  diff(2x+1 < 1, x)"
        ,"       ^^^^^^^^"
        ,""
        ,"I expected a real number here, but the result of a comparison is a truth value."])

    it "differentiates |x|, re, im and conj with respect to a real variable" $ do
      parseR 3 "diff(|x|, x)" `shouldBe` Right 1
      parseR (-2) "diff(|x|, x)" `shouldBe` Right (-1)

    it "take the Wirtinger derivative of what isn't holomorphic" $ do
      -- ∂ = (∂/∂x - i ∂/∂y)/2: ∂ re z = 1/2, ∂ im z = -i/2, ∂ conj z = 0
      parseC 1 "diff(re z, z)" `shouldBeNear` 0.5
      parseC 1 "diff(im z, z)" `shouldBeNear` (0 :+ (-0.5))
      parseC 1 "diff(conj z, z)" `shouldBeNear` 0
      -- ∂|z|² = z̄, ∂|z| = z̄ / (2|z|), and 0 at 0
      parseC (1 :+ 2) "diff(|z|^2, z)" `shouldBeNear` (1 :+ (-2))
      parseC (3 :+ 4) "diff(|z|, z)" `shouldBeNear` (0.3 :+ (-0.4))
      parseC 0 "diff(|z|, z)" `shouldBeNear` 0
      -- ∂ arg z at i: arg's x-derivative is -1 there, its y-derivative 0
      parseC (0 :+ 1) "diff(arg z, z)" `shouldBeNear` (-0.5)
      -- through a holomorphic function: ∂ exp(|z|) = exp(|z|) z̄ / (2|z|)
      parseC (3 :+ 4) "diff(exp(|z|), z)" `shouldBeNear` (exp 5 * (0.3 :+ (-0.4)))
      -- and through what isn't: ∂ |conj z|² = z̄ too
      parseC (1 :+ 2) "diff(|conj(z)|^2, z)" `shouldBeNear` (1 :+ (-2))
      parseC (1 :+ 2) "diff(re(z)^2 + im(z)^2, z)" `shouldBeNear` (1 :+ (-2))

    it "give diff_bar, the derivative with respect to z̄" $ do
      parseC (1 :+ 2) "diff_bar(z^2, z)" `shouldBeNear` 0
      parseC (1 :+ 2) "diff_bar(conj z, z)" `shouldBeNear` 1
      parseC (1 :+ 2) "diff_bar(|z|^2, z)" `shouldBeNear` (1 :+ 2)
      parseC (1 :+ 2) "diff_bar(re z, z)" `shouldBeNear` 0.5
      parseC (1 :+ 2) "diff_bar(exp(conj(z)), z)" `shouldBeNear` exp (1 :+ (-2))

    it "differentiate what jumps where it doesn't, and |x| at its kink" $ do
      parseR 2.5 "diff(floor(x) + round(x) + ceiling(x), x)" `shouldBe` Right 0
      parseR 2.5 "diff(mod(x^2, 1.0), x)" `shouldBe` Right 5
      parseR 2.5 "diff(x + round(x) // 2, x)" `shouldBe` Right 1
      -- (Euclidean: x mod b = x - |b| floor(x / |b|))
      parseR 2.5 "diff(mod(7.0, x), x)" `shouldBe` Right (-2)
      parseR (-2) "diff(|x|, x)" `shouldBe` Right (-1)
      parseR 0 "diff(|x|, x)" `shouldBe` Right 0

    it "throws errors when the variable isn't a variable" $ do
      parseR 0 "diff(x, 2)" `shouldBe` Left (unlines
        ["  diff(x, 2)"
        ,"          ^"
        ,""
        ,"A derivative must be taken with respect to a real or complex variable."])
      parseC 1 "diff(z, z^2)" `shouldBe` Left (unlines
        ["  diff(z, z^2)"
        ,"          ^^^"
        ,""
        ,"A derivative must be taken with respect to a real or complex variable."])
