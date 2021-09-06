{-# language QuasiQuotes #-}
module Language.Code.ParserSpec (spec) where

import Test.Hspec
import Language.Type
import Language.Value
import Language.Value.Parser
import Language.Code.Parser
import Language.Code.Simulator

import Control.Monad.State
--import Text.RawString.QQ

runEmpty :: ScalarProxy t -> String -> Either (Int, BadParse) (ScalarType t)
runEmpty t input = fmap ((`evalState` EmptyContext ) . simulate) $ parseCode EmptyEnvProxy t input

runWithX :: forall t ty. ScalarProxy t -> Scalar ty -> String -> Either (Int, BadParse) (ScalarType t)
runWithX t (Scalar xt x) input = withKnownType xt $
  let env = BindingProxy (Proxy @"x") xt (Proxy @'[])
      ctx = Bind (Proxy @"x") xt x EmptyContext
  in fmap ((`evalState` ctx) . simulate) $ parseCode env t input

runWithXY :: forall t xt yt. ScalarProxy t -> Scalar xt -> Scalar yt -> String -> Either (Int, BadParse) (ScalarType t)
runWithXY t (Scalar xt x) (Scalar yt y) input = withKnownType xt $ withKnownType yt $
  let ctx = Bind (Proxy @"x") xt x
          $ Bind (Proxy @"y") yt y
          $ EmptyContext
  in fmap ((`evalState` ctx) . simulate) $ parseCode (envProxy Proxy) t input

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

       let p1 = "set x to 5\ninit y to x - 2\nif true then\n  set x to 2 * y\nelse\n  pass\nx"

       runWithX IntegerProxy (Scalar IntegerProxy 0) p1 `shouldBe` Right 6
