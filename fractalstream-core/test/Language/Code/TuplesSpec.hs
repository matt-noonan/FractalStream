{-# language QuasiQuotes #-}
-- | Tuple assignments, declarations, and iterations, and tuples of
-- numbers in @escapes@, @vanishes@, @stops@, and @=@.
module Language.Code.TuplesSpec (spec) where

import Test.Hspec

import FractalStream.Prelude
import Data.List (isInfixOf)

import Language.Type
import Language.Value
import Language.Value.Typecheck
import Language.Code.Parser
import Language.Code.Simulator
import Language.Draw

import Text.RawString.QQ

-- | Run code with real variables @x@ and @y@ starting at the given
-- values, an escape radius of 10, and a vanishing radius of 0.0001, and
-- give back @x@ and @y@ afterwards.
runXY :: (Double, Double) -> String -> Either String (Double, Double)
runXY (x, y) input =
  let ctx = Bind (Proxy @"x") RealType x
          $ Bind (Proxy @"y") RealType y
          $ Bind (Proxy @InternalEscapeRadius) RealType 10
          $ Bind (Proxy @InternalVanishingRadius) RealType 0.0001
          $ Bind (Proxy @InternalIterations) IntegerType 0
          $ Bind (Proxy @InternalIterationLimit) IntegerType 100
          $ Bind (Proxy @InternalStuck) BooleanType False
          $ EmptyContext
      get' = (,) <$> eval (Var (Proxy @"x") RealType bindingEvidence)
                 <*> eval (Var (Proxy @"y") RealType bindingEvidence)
  in first (`ppFullError` input)
     $ fmap ((`evalState` (ctx, ())) . (\c -> simulate noDraw c >> get'))
     $ parseCode (envProxy Proxy) noCommonDefs input

noDraw :: DrawHandler (HaskellTypeM ())
noDraw = DrawHandler (const $ pure ())

shouldGive :: Either String (Double, Double) -> (Double, Double) -> Expectation
shouldGive result (ex, ey) = case result of
  Left err -> expectationFailure err
  Right (x, y) -> do
    x `shouldSatisfy` (\v -> abs (v - ex) < 1e-6)
    y `shouldSatisfy` (\v -> abs (v - ey) < 1e-6)

shouldFailWith :: Either String a -> String -> Expectation
shouldFailWith result needle = case result of
  Left err -> err `shouldSatisfy` isInfixOf needle
  Right _ -> expectationFailure ("expected an error mentioning " ++ show needle)

spec :: Spec
spec = do

  describe "tuple assignment" $ do

    it "sets every variable at once" $
      runXY (1, 2) "x, y <- y, x" `shouldGive` (2, 1)

    it "takes parentheses around both sides" $
      runXY (3, 1) "(x, y) <- (x + y, x - y)" `shouldGive` (4, 2)

    it "converts each part, as an ordinary assignment does" $
      runXY (0, 0) "x, y <- 1, 2" `shouldGive` (1, 2)

    it "takes apart a pair-valued expression, nested patterns and all" $
      runXY (1, 2) [r|
p := (x + 1, (y, 7.0))
w := 0.0
(x, (y, w)) <- p
y <- y + w
|] `shouldGive` (2, 9)

    it "nests longer tuples to the right, and reads pairs inside if/then/else" $ do
      runXY (1, 2) "a, b, c := 1.0, 2.0, 3.0\nx, y <- a + b, c" `shouldGive` (3, 3)
      runXY (1, 0) "x, y <- if x > 0 then 1, 2 else 3, 4" `shouldGive` (1, 2)
      runXY (-1, 0) "x, y <- if x > 0 then 1, 2 else 3, 4" `shouldGive` (3, 4)

    it "rejects a variable that appears twice" $
      runXY (0, 0) "x, x <- 1, 2" `shouldFailWith` "more than once"

    it "rejects a value that isn't a pair" $
      runXY (0, 0) "x, y <- 3" `shouldFailWith` "a pair"

    it "needs its variables to be declared already" $
      runXY (0, 0) "x, q <- 1, 2" `shouldFailWith` "No variable named q"

  describe "tuple declaration" $ do

    it "declares each variable, in scope for the rest of the block" $
      runXY (1, 2) [r|
a, b, c := x, y, x + y
x, y <- b + c, a
|] `shouldGive` (5, 1)

    it "takes apart a pair-valued expression" $
      runXY (1, 2) [r|
q := (y, x)
u, v := q
x, y <- u, v
|] `shouldGive` (2, 1)

    it "infers each variable's type on its own" $
      runXY (0, 0) [r|
n, m := 1, 2
n <- n + 0.5
x, y <- n, m
|] `shouldGive` (1.5, 2)

  describe "tuple iteration" $ do

    it "iterates a pair of variables" $
      runXY (0, 1) "iterate (x, y) -> (x + 1, 2 y) until x ≥ 5" `shouldGive` (5, 32)

    it "measures a tuple's escape by its length" $ do
      -- |(7, 8)| > 10, though neither part is
      runXY (7, 8) "iterate (x, y) -> (x / 2, y / 2) until (x, y) escapes up to 3 times"
        `shouldGive` (7, 8)
      runXY (6, 7) "iterate (x, y) -> (x / 2, y / 2) until (x, y) escapes up to 3 times"
        `shouldGive` (0.75, 0.875)

    it "measures a tuple's vanishing by its length" $
      runXY (1, 1) "iterate (x, y) -> (x / 10, y / 10) until (x, y) vanishes"
        `shouldGive` (0.00001, 0.00001)

    it "stops when a tuple settles down" $ case runXY (0, 1) "iterate (x, y) -> (x / 2 + 1, y / 2) until (x, y) stops" of
      Left err -> expectationFailure err
      Right (x, y) -> do
        abs (x - 2) `shouldSatisfy` (< 0.001)
        abs y `shouldSatisfy` (< 0.001)

  describe "comparing tuples of numbers" $ do

    it "compares them by the distance between them" $ do
      runXY (1, 2) "if (x, y) = (1, 2.00001):\n  x <- 100\n" `shouldGive` (100, 2)
      runXY (1, 2) "if (x, y) = (1, 2.1):\n  x <- 100\n" `shouldGive` (1, 2)
      runXY (1, 2) "if (x, y) ≠ (1, 2.1):\n  x <- 100\n" `shouldGive` (100, 2)

    it "rejects tuples of other things" $
      runXY (0, 0) "if (x, true) escapes:\n  pass\n" `shouldFailWith` "tuple of numbers"
