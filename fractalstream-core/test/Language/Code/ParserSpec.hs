{-# language QuasiQuotes #-}
module Language.Code.ParserSpec (spec) where

import Test.Hspec
import Language.Type
import Language.Value
import Language.Value.Parser
import Language.Code.Parser
import Language.Code.Simulator
import Language.Effect
import Data.Color

import Control.Monad.State
import Text.RawString.QQ

runEmpty :: ScalarProxy t
         -> String
         -> Either (Int, BadParse) (ScalarType t)
runEmpty t input
  = fmap ((`evalState` (EmptyContext, ()) ) . simulate NoHandler)
    $ parseCode (EP NoEffs) EmptyEnvProxy t input

runWithX :: forall t xt
          . ScalarProxy t
         -> Scalar xt
         -> String
         -> Either (Int, BadParse) (ScalarType t)
runWithX t (Scalar xt x) input = withKnownType xt $
  let env = BindingProxy (Proxy @"x") xt EmptyEnvProxy
      ctx = Bind (Proxy @"x") xt x EmptyContext
  in fmap ((`evalState` (ctx, ())) . simulate NoHandler)
   $ parseCode (EP NoEffs) env t input

runWithXY :: forall t xt yt
           . ScalarProxy t
          -> Scalar xt
          -> Scalar yt
          -> String
          -> Either (Int, BadParse) (ScalarType t)
runWithXY t (Scalar xt x) (Scalar yt y) input = withKnownType xt $ withKnownType yt $
  let ctx = Bind (Proxy @"x") xt x
          $ Bind (Proxy @"y") yt y
          $ EmptyContext
  in fmap ((`evalState` (ctx, ())) . simulate NoHandler)
   $ parseCode (EP NoEffs) (envProxy Proxy) t input

spec :: Spec
spec = do

  describe "when parsing code blocks" $ do

    it "can parse if/then/else blocks" $ do

      let p1 = "if true then\n  pass\n  1 + 2\nelse\n  3 + 4"
          p2 = "set x to 1 + 3\nx"
          p3 = "if y then\n  set x to 1 + 3\n  pass\nelse\n  pass\nx"
      runEmpty  IntegerProxy p1 `shouldBe` Right 3
      runWithX  IntegerProxy (Scalar IntegerProxy 7) p2 `shouldBe` Right 4
      runWithXY IntegerProxy (Scalar IntegerProxy 7) (Scalar BooleanProxy True)  p3 `shouldBe` Right 4
      runWithXY IntegerProxy (Scalar IntegerProxy 7) (Scalar BooleanProxy False) p3 `shouldBe` Right 7

    it "can bind new variables" $ do

       let p1 = [r|
set x to 5
init y : Z to x - 2
if true then
  set x to 2 * y
else
  pass
x|]
           p2 = [r|
set x to 5
init y : Z to x - 2
if true then
  set x to 2 * y
x|]

       runWithX IntegerProxy (Scalar IntegerProxy 0) p1 `shouldBe` Right 6
       runWithX IntegerProxy (Scalar IntegerProxy 0) p2 `shouldBe` Right 6

    it "can coerce variable types" $ do
        let p1 = [r|
init k : Z to 1
set x to k|]
        runWithX VoidProxy (Scalar RealProxy 0) p1 `shouldBe` Right ()

  describe "when parsing more complex code" $ do

    it "can parse a checkered Mandelbrot program" $ do
      let _mandel = [r|
init C : C to x + y i
init z : C to 0
init count : Z to 0
loop
  set z to z z + C
  set count to count + 1
  |z| < 10 and count < 100
if count = 100 then
  black
else if im z > 0 then
  red
else
  yellow|]
      let mandel = [r|
init C : C to x + y i
init z : C to 0
init count : Z to 0
loop
  set z to z z + C
  set count to count + 1
  |z| < 100 and count < 100
if count = 100 then
  black
else if im z > 0 then
  red
else
  yellow|]

          runMandel (x :+ y) = runWithXY ColorProxy (Scalar RealProxy x) (Scalar RealProxy y) mandel
      runMandel 0 `shouldBe` Right black
      runMandel (1 :+ 1) `shouldBe` Right yellow
      runMandel (1 :+ (-1)) `shouldBe` Right red
