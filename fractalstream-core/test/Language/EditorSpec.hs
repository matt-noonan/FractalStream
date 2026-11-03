module Language.EditorSpec (spec) where

import Test.Hspec

import Language.Editor

spec :: Spec
spec = describe "editing scripts" $ do
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

  describe "pasting" $ do

    it "pastes a single line as it is" $
      pasteIndented "    x <- " "f(z)  " `shouldBe` "f(z)  "

    it "moves whole lines to the indentation they're pasted at" $ do
      pasteIndented "    " "  a\n    b\n  c\n" `shouldBe` "a\n      b\n    c\n"
      pasteIndented "" "      a\n      b" `shouldBe` "a\nb"

    it "infers the first line's indentation when the copy started at its code" $ do
      -- copied from `    until z escapes:` onwards
      pasteIndented "  " "until z escapes:\n      z <- f(z)\n    erase" `shouldBe`
        "until z escapes:\n    z <- f(z)\n  erase"
      -- copied from `    z <- f(z)` onwards
      pasteIndented "  " "z <- f(z)\n    erase\n  done" `shouldBe` "z <- f(z)\n  erase\ndone"

    it "lines pasted after code follow that line's indentation" $
      pasteIndented "  x <- 1 " "+ 2\n  y <- 3" `shouldBe` "+ 2\n  y <- 3"

    it "keeps blank lines blank, and ignores carriage returns" $
      pasteIndented "  " "a:\r\n\r\n  b\r\n" `shouldBe` "a:\n\n    b\n"

  describe "commenting" $ do

    it "comments out lines, lined up at their shallowest indentation" $
      toggleComments ["  if x:", "    y", "", "  z"] `shouldBe`
        ["  # if x:", "  #   y", "", "  # z"]

    it "uncomments lines that are all comments" $ do
      toggleComments ["  # if x:", "  #   y", "", "  #z"] `shouldBe` ["  if x:", "    y", "", "  z"]
      toggleComments ["  # a", "  b"] `shouldBe` ["  # # a", "  # b"]

    it "starts a comment on a blank line" $
      toggleComments ["    "] `shouldBe` ["    # "]
