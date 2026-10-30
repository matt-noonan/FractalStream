{-# language QuasiQuotes #-}

module Language.Effect.ListSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Environment
import Language.Code.Parser
import Language.Code.Simulator
import Language.Draw (DrawHandler(..))
import Language.Value

import Text.RawString.QQ

runWithXY :: Double
          -> Double
          -> [Double]
          -> String
          -> Either String [Double]
runWithXY x y lst input = do
  let ctx = Bind (Proxy @"x") RealType x
          $ Bind (Proxy @"y") RealType y
          $ Bind (Proxy @"test") (ListType RealType) lst
          $ EmptyContext
  testVar <- case lookupEnv (Proxy @"test") (ListType RealType) (contextToEnv ctx) of
    Found pf -> pure $ Var (Proxy @"test") (ListType RealType) pf
    _ -> Left "impossible"
  first (`ppFullError` input)
    $ fmap ((`evalState` (ctx, ())) . (\c -> simulate noDraw c >> eval testVar))
    $ parseCode (envProxy Proxy) noCommonDefs input

noDraw :: DrawHandler (HaskellTypeM ())
noDraw = DrawHandler (const $ pure ())


spec :: Spec
spec = do

  describe "when using list operations in code" $ do
    it "can parse list operations" $ do

      let p1 = [r|
x <- 0
for each item in test:
  x <- x + item
y <- x
test <- append(test, y)|]
      runWithXY 5 7 [1,2,3,4,5] p1 `shouldBe` Right [1,2,3,4,5,15]

      let p2 = [r|
x <- 3
test <- remove(test, item -> item <= x)|]
      runWithXY 5 7 [1,2,3,4,5] p2 `shouldBe` Right [4,5]

      let p3 = [r|
x <- 3
if |x - y| < 2:
  test <- []|]

      runWithXY 1 10 [1,2,3,4,5] p3 `shouldBe` Right [1,2,3,4,5]
      runWithXY 1 4 [1,2,3,4,5] p3  `shouldBe` Right []

    it "indexes before comparing or adding" $ do
      let p1 = [r|
x <- 0
if test @ 2 > 1.5 and test @@ 0 > 4.5:
  x <- test @ 1 + 10
test <- [x]|]
      runWithXY 0 0 [1,2,3,4,5] p1 `shouldBe` Right [11]

    it "loops over list expressions, not just list variables" $ do
      let p1 = [r|
x <- 0
for each a in [1, 2, 3]:
  for each b in range(1, 2):
    x <- x + a b
for each a in append(test, 10):
  x <- x + a
test <- [x]|]
      runWithXY 0 0 [1,2] p1 `shouldBe` Right [31]

      let p2 = [r|
x <- 0
with first a matching a > 2 in transform(test, t -> t + 1):
  x <- a
else:
  x <- -1
test <- [x]|]
      runWithXY 0 0 [1,2,3] p2 `shouldBe` Right [3]

