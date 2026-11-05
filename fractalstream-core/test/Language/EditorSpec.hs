module Language.EditorSpec (spec) where

import Test.Hspec

import Language.Editor

spec :: Spec
spec = describe "editing scripts" $ do
  describe "whole characters" $ do

    it "are a letter and the combining marks on it" $ do
      lastCharacterStart "z + x\x302" `shouldBe` 4
      lastCharacterStart "z + x\x307\x302" `shouldBe` 4
      lastCharacterStart "z + x" `shouldBe` 4
      firstCharacterLength "x\x307\x302 + z" `shouldBe` 3
      firstCharacterLength "x + z" `shouldBe` 1

    it "are combining marks, with nothing under them" $ do
      lastCharacterStart "\x302\x303" `shouldBe` 0
      firstCharacterLength "\x302\x303 + z" `shouldBe` 2
      lastCharacterStart "" `shouldBe` 0
      firstCharacterLength "" `shouldBe` 0

  describe "the cursor" $ do

    it "isn't left inside a whole character" $ do
      let line = "z + x\x307\x20D7 + 1"
      characterBoundary line 5 True `shouldBe` 4
      characterBoundary line 6 True `shouldBe` 4
      characterBoundary line 5 False `shouldBe` 7
      characterBoundary line 6 False `shouldBe` 7

    it "stays anywhere else" $ do
      let line = "z + x\x307\x20D7 + 1"
      characterBoundary line 4 True `shouldBe` 4
      characterBoundary line 7 False `shouldBe` 7
      characterBoundary line 0 True `shouldBe` 0
      characterBoundary line (length line) False `shouldBe` length line

  describe "what to underline" $ do

    it "is the characters asked for" $
      visibleSpan "x + y" 2 3 `shouldBe` Just (2, 3)

    it "takes in a letter's combining marks, and the letter under them" $ do
      visibleSpan "x\x302 + 1" 0 1 `shouldBe` Just (0, 2)
      visibleSpan "x\x302 + 1" 1 2 `shouldBe` Just (0, 2)

    it "goes on to something that shows, for marks on nothing" $ do
      visibleSpan "\x302 + 1" 0 1 `shouldBe` Just (0, 2)
      visibleSpan "ab\x302\x303" 3 4 `shouldBe` Just (1, 4)
      visibleSpan "\x200B" 0 1 `shouldBe` Nothing

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

  describe "deleting the previous token" $ do

    let deletes line = drop (deleteBackTo line) line

    it "deletes the token before the cursor, and the spaces after it" $ do
      deletes "iterate z ⟼ f(z) until z escapes" `shouldBe` "escapes"
      deletes "  x ⟵ " `shouldBe` "⟵ "
      deletes "  C ⟵ -0.11 + 0.78i" `shouldBe` "0.78i"
      deletes "  w ⟵ f(z)" `shouldBe` ")"
      deletes "  write \"done\"" `shouldBe` "\"done\""
      deletes "  z ⟵ z²" `shouldBe` "²"

    it "deletes words in comments and unfinished strings" $ do
      deletes "  z ⟵ 1  # the start value" `shouldBe` "value"
      deletes "  write \"hello wor" `shouldBe` "wor"

    it "deletes indentation back to the start of the line" $ do
      deleteBackTo "    " `shouldBe` 0
      deleteBackTo "" `shouldBe` 0
