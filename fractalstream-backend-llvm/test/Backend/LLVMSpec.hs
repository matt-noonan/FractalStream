{-# language QuasiQuotes #-}
module Backend.LLVMSpec (spec) where

import Test.Hspec

import Language.Value
import Data.Color
import Backend.LLVM

import Text.RawString.QQ

spec :: Spec
spec = do

  describe "when compiling programs to LLVM" $ do

    it "generates the expected code for simple programs" $ do
      let env = declare @"x" RealProxy
              $ declare @"y" RealProxy
              $ endOfDecls
      let prog1 = [r|
loop
  set x to x + 1
  x < y
x y
|]
      withCompiledCode prog1 $ \f -> do
        f1 <- invoke' env RealProxy f 1.5 5
        f2 <- invoke' env RealProxy f 1.5 2
        f1 `shouldBe` (5.5 * 5)
        f2 `shouldBe` (2.5 * 2)

    it "can compile a basic Mandelbrot set program" $ do
      let env = declare @"C" ComplexProxy
              $ declare @"maxRadius" RealProxy
              $ declare @"maxIter" IntegerProxy
              $ endOfDecls
      let mandel = [r|
init z : C to 0
init k : Z to 0
loop
  set z to z z + C
  set k to k + 1
  re(z) re(z) + im(z) im(z) < maxRadius maxRadius and k < maxIter
if k = maxIter then
  blue
else
  red|]

      withCompiledCode mandel $ \f -> do
        v1 <- invoke' env ColorProxy f (1 :+ 1) 10 100
        v1 `shouldBe` red
        v2 <- invoke' env ColorProxy f ((-1) :+ 0.1) 10 100
        v2 `shouldBe` blue
        v3 <- invoke' env ColorProxy f ((-0.123) :+ 0.745) 10 100
        v3 `shouldBe` blue
