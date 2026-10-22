module Backend.LLVMSpec (spec) where

import Test.Hspec

import Language.Value
import Data.Color
import Backend.LLVM

-- Deliberately plain string literals here, not a 'Text.RawString.QQ'
-- quasiquote: quasiquotes require Template Haskell, which forces GHC to
-- load this module's own transitive dependencies -- including
-- fractalstream-backend-llvm itself, and so llvm-hs's statically-linked,
-- every-target LLVM archive -- through its runtime (bytecode) linker at
-- compile time. That linker doesn't fold the duplicate weak symbols
-- LLVM's per-target code generators share, so it fails outright; a
-- normal compiled test executable never hits this since it links via
-- the system linker instead. See CLAUDE_NOTES.md.

spec :: Spec
spec = do

  describe "when compiling programs to LLVM" $ do

    it "generates the expected code for simple programs" $ do
      let env = declare @"x" RealType
              $ declare @"y" RealType
              $ declare @"color" ColorType
              $ declare @"[internal] iteration count" IntegerType
              $ declare @"[internal] iteration limit" IntegerType
              $ declare @"[internal] stuck" BooleanType
              $ declare @"[internal] escape radius" RealType
              $ declare @"[internal] vanishing radius" RealType
              $ endOfDecls
      let prog1 = "\nwhile x < y up to 1000 times:\n  x <- x + 1\ncolor <- rgb(y / x, 0, 0)\n"
      withCompiledCode (Proxy @"color") ColorType env prog1 $ \f -> do
        f1 <- invoke' env ColorType f 1.5 5 black 0 0 False 0 0
        f2 <- invoke' env ColorType f 1.5 2 black 0 0 False 0 0
        f1 `shouldBe` (rgbToColor (round (255 * 5 / 5.5 :: Double), 0, 0))
        f2 `shouldBe` (rgbToColor (round (255 * 2 / 2.5 :: Double), 0, 0))

    it "can compile a basic Mandelbrot set program" $ do
      let env = declare @"x" RealType
              $ declare @"y" RealType
              $ declare @"maxRadius" RealType
              $ declare @"maxIter" IntegerType
              $ declare @"color" ColorType
              $ declare @"[internal] iteration count" IntegerType
              $ declare @"[internal] iteration limit" IntegerType
              $ declare @"[internal] stuck" BooleanType
              $ declare @"[internal] escape radius" RealType
              $ declare @"[internal] vanishing radius" RealType
              $ endOfDecls
      let mandel = "\nC : C <- x + i y\nz : C <- 0\nk : Z <- 0\n\
                   \while |z|^2 < maxRadius^2 and k < maxIter up to 1000 times:\n\
                   \  z <- z^2 + C\n\
                   \  k <- k + 1\n\
                   \if k = maxIter:\n\
                   \  color <- blue\n\
                   \else:\n\
                   \  color <- red"

      withCompiledCode (Proxy @"color") ColorType env mandel $ \f -> do
        v1 <- invoke' env ColorType f 1 1 10 100 black 0 0 False 0 0
        v1 `shouldBe` red
        v2 <- invoke' env ColorType f (-1) 0.1 10 100 black 0 0 False 0 0
        v2 `shouldBe` blue
        v3 <- invoke' env ColorType f (-0.123) 0.745 10 100 black 0 0 False 0 0
        v3 `shouldBe` blue
