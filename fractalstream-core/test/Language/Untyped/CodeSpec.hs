{-# language QuasiQuotes #-}
module Language.Untyped.CodeSpec (spec) where

import Test.Hspec
import Text.RawString.QQ

import Language.Code.Parser
import Language.Untyped.Code
import Language.Untyped.Value
import Data.Recursive

import qualified Data.Set as Set

spec :: Spec
spec = do

  describe "When setting variables" $ do

    it "Converts unbound set instructions to let-bindings" $ do
      let prog1 = [r|
set z to 0
set w to 1
set w to 2
set z to 3
|]
          prog2 = [r|
set z to 0
if z = 0 then
  set w to 1
  set z to 2
else
  set z to 3
  set w to 4
|]

      (promoteSetToLet Set.empty <$> uParseCode prog1)
          `shouldBe` Right (Fix (Let "z" (Fix (ConstI 0)) (Fix (Let "w" (Fix (ConstI 1)) (Fix (Block [Fix (Set "w" (Fix (ConstI 2))),Fix (Set "z" (Fix (ConstI 3)))]))))))

      (promoteSetToLet Set.empty <$> uParseCode prog2)
          `shouldBe` Right (Fix (Let "z" (Fix (ConstI 0)) (Fix (IfThenElse (Fix (Eql (Fix (Var "z")) (Fix (ConstI 0)))) (Fix (Let "w" (Fix (ConstI 1)) (Fix (Set "z" (Fix (ConstI 2)))))) (Fix (Block [Fix (Set "z" (Fix (ConstI 3))),Fix (Let "w" (Fix (ConstI 4)) (Fix (Block [])))]))))))
