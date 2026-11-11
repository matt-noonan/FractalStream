{-# language QuasiQuotes #-}
-- | Splitting a viewer's code into the part that's the same for every
-- pixel and the rest ("Language.Code.Hoist").
module Language.Code.HoistSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Type
import Language.Code.Syntax
import Language.Code.Parser (parseParsedCode, noCommonDefs)
import Language.Code.Typecheck (expandParsedCode)
import Language.Code.Hoist
import Language.Value.Typing (strictTypeEnv)
import Language.Value.Builtins (internalIterations, internalStuck, internalIterationLimit
                               , internalEscapeRadius, internalVanishingRadius)

import Text.RawString.QQ

-- | Split code for a viewer of @z@, with the configuration values @c@ (a
-- complex number) and @m@ (an integer).
split :: String -> Maybe Split
split src = case parseParsedCode noCommonDefs src of
  Left _ -> error "the code doesn't parse"
  Right code -> case expandParsedCode tenv code of
    Left _ -> error "the code doesn't check"
    Right expanded -> splitHoistable (Set.fromList ["z", "color"]) expanded
  where
    tenv = strictTypeEnv $ Map.fromList
      [ ("z", ComplexT), ("c", ComplexT), ("m", IntegerT), ("color", ColorT)
      , (internalIterations, IntegerT), (internalStuck, BooleanT)
      , (internalIterationLimit, IntegerT), (internalEscapeRadius, RealT)
      , (internalVanishingRadius, RealT) ]

-- | The names of a split's results, and the top-level statements of its
-- prefix and rest, roughly (each one's kind, and the name it declares or
-- sets).
outputsOf :: Maybe Split -> [String]
outputsOf = maybe [] (map fst . splitOutputs)

restOf :: Maybe Split -> [String]
restOf = maybe [] (topLevel . splitRest)

topLevel :: Stmt -> [String]
topLevel s0@(Stmt _ node) = case node of
  SBlock body -> map kind body
  _ -> [kind s0]
  where
    kind (Stmt _ n) = case n of
      SDecl v _ _ -> "decl " ++ v
      SSet v _ -> "set " ++ v
      SFun f _ _ -> "fun " ++ f
      SIf{} -> "if"
      SLoop{} -> "loop"
      SIterate{} -> "iterate"
      SBlock{} -> "block"
      _ -> "other"

spec :: Spec
spec = describe "splitting off the work that's the same for every pixel" $ do

  it "takes what depends only on the configuration, and hands it on" $ do
    let s = split [r|
a := c^2 + m
w := z + a
color <- if |w| < 1 then black else white
|]
    outputsOf s `shouldBe` ["[hoisted] a"]
    -- (each pixel starts with its own copy of the result)
    restOf s `shouldBe` ["decl a", "decl w", "set color"]

  it "leaves what depends on the pixel, directly or not" $ do
    split [r|
w := z^2
b := w + c
color <- if |b| < 1 then black else white
|] `shouldSatisfy` isNothing

  it "takes work from after code that depends on the pixel, if moving it changes nothing" $ do
    let s = split [r|
w := z^2
a := c + 1
color <- if |w + a| < 1 then black else white
|]
    outputsOf s `shouldBe` ["[hoisted] a"]
    restOf s `shouldBe` ["decl a", "decl w", "set color"]

  it "doesn't move work ahead of code that reads what it sets" $ do
    let s = split [r|
n := 0
w := z + n
n <- 5
color <- if |w| < n then black else white
|]
    outputsOf s `shouldBe` ["[hoisted] n"]
    restOf s `shouldBe` ["decl n", "decl w", "set n", "set color"]

  it "doesn't set the configuration's values, which every pixel starts from" $
    split [r|
c <- c + 1
color <- if |z - c| < 1 then black else white
|] `shouldSatisfy` isNothing

  it "doesn't take anything from inside an if that depends on the pixel" $
    split [r|
if |z| < 1:
  a := c^2
  color <- if |a| < 1 then black else white
else:
  color <- white
|] `shouldSatisfy` isNothing

  it "doesn't take a use of an autocolor table" $
    split [r|
k := autocolor(1, c)
color <- if |z| < 1 then k else white
|] `shouldSatisfy` isNothing

  it "hands on its loops' iteration count" $ do
    let s = split [r|
k := 0
while k < m:
  k <- k + 1
color <- if iterations = 3 and |z| < 1 then black else white
|]
    outputsOf s `shouldBe` [hoistedIterations, hoistedStuck]

  it "lets both parts use a function that doesn't depend on the pixel" $ do
    let s = split [r|
f(t) := t^2 + c
a := f(c)
color <- if |f(z) + a| < 1 then black else white
|]
    outputsOf s `shouldBe` ["[hoisted] a"]
    restOf s `shouldBe` ["decl a", "fun f", "set color"]

  it "looks into blocks, renaming a variable declared elsewhere too" $ do
    let s = split [r|
define setup:
  a := c^2
  b := a + 1
run setup
if |z| < 1:
  a := z
  color <- if |a| < 1 then black else white
else:
  color <- if |b| < 1 then black else white
|]
    -- (`a` is only needed in the prefix; `b`, by the rest)
    outputsOf s `shouldBe` ["[hoisted] b"]
    restOf s `shouldBe` ["decl b", "if"]
