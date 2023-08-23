{-# language QuasiQuotes #-}
module Language.Untyped.CodeSpec (spec) where

import Test.Hspec

spec :: Spec
spec = pure ()

{- TODO?
import Text.RawString.QQ

import Language.Parser
import Language.Code.Parser
import Language.Untyped.Code
import Language.Untyped.Value
import Data.Recursive
import Debug.Trace

import qualified Data.Set as Set
import Control.Monad

spec :: Spec
spec = do

  describe "When setting variables" $ do

    it "Converts unbound set instructions to let-bindings" $ do
      let prog1 = [r|
set p to 0
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

      traceM ("prog1 = " ++ show prog1)
      traceM ("tokens = " ++ show (tokenizeWithIndentation prog1))
      when True $ do
        let parsed1 = uParseCode prog1
        traceM ("parsed1 = " ++ show parsed1)
        (promoteSetToLet Set.empty <$> parsed1)
          `shouldBe` Right (Fix (Let "z" (Fix (ConstI 0)) (Fix (Let "w" (Fix (ConstI 1)) (Fix (Block [Fix (Set "w" (Fix (ConstI 2))),Fix (Set "z" (Fix (ConstI 3)))]))))))

        (promoteSetToLet Set.empty <$> uParseCode prog2)
          `shouldBe` Right (Fix (Let "z" (Fix (ConstI 0)) (Fix (IfThenElse (Fix (Eql (Fix (Var "z")) (Fix (ConstI 0)))) (Fix (Let "w" (Fix (ConstI 1)) (Fix (Set "z" (Fix (ConstI 2)))))) (Fix (Block [Fix (Set "z" (Fix (ConstI 3))),Fix (Let "w" (Fix (ConstI 4)) (Fix (Block [])))]))))))
-}
