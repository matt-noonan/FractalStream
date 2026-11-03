module Language.EditorSpec (spec) where

import Test.Hspec

import Language.Editor

spec :: Spec
spec = describe "editing scripts" $
  describe "the indentation of a new line" $ do

    it "keeps the line's indentation" $ do
      newLineIndent "z <- z^2 + C" `shouldBe` 0
      newLineIndent "    draw point at z" `shouldBe` 4

    it "indents further after a line that opens a block" $ do
      newLineIndent "on click:" `shouldBe` 2
      newLineIndent "  until z escapes:" `shouldBe` 4
      newLineIndent "  if z escaped:   " `shouldBe` 4

    it "doesn't count a comment, or a colon in quotes" $ do
      newLineIndent "  repeat:  # forever" `shouldBe` 4
      newLineIndent "  write \"x:\" at z" `shouldBe` 2
      newLineIndent "  write \"#:\" at z # done:" `shouldBe` 2
      newLineIndent "  write \"a \\\"b:\" at z" `shouldBe` 2
