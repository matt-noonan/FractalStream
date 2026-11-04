{-# language QuasiQuotes #-}
module Language.Editor.HighlightSpec (spec) where

import Test.Hspec
import Text.RawString.QQ

import Data.List (isPrefixOf, tails)
import qualified Data.Map as Map

import Language.Editor.Highlight

-- | The class of the @n@th (from 0) occurrence of a word in a script.
classOf :: String -> String -> Int -> Maybe Highlight
classOf src word n =
  case drop n [ i | (i, rest) <- zip [0 ..] (tails src), word `isPrefixOf` rest, wordStart i
                   , wordEnd (drop (length word) rest) ] of
    i : _ -> case [ h | (from, to, h) <- highlights src, from <= i, i < to ] of
      h : _ -> Just h
      [] -> Nothing
    [] -> error ("no occurrence " ++ show n ++ " of " ++ show word)
  where
    wordStart i = i == 0 || src !! (i - 1) `notElem` wordChars
    wordEnd rest = case rest of
      ch : _ -> ch `notElem` wordChars || not (last word `elem` wordChars)
      [] -> True
    wordChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

project :: String
project = [r|@config "Parameters"
C : C as "C =" <- -0.12 + 0.75i
steps : Z <- 20

@common
let f(z) = z^2 + C
define shade(c):
  color <- blend(0.5, light c, dark c)

@viewer "Julia set" pixelSize=1/150
iterate z -> f(z) until z escapes  # the usual
if stuck:
  run shade(blue)
else:
  color <- rainbow(iterations / steps)

@viewer "Parameter plane" var=C
z := 0
iterate z -> f(z) until z escapes
color <- if stuck then black else white

@tool "Trace" for="Julia set"
radius : R <- 0.1
on click-or-drag at w:
  erase
  use yellow for stroke
  line := w
  draw circle at w with radius radius
  draw point at mystery
  for each p in [w, z]:
    draw line from p to f(p)
|]

spec :: Spec
spec = describe "highlighting scripts" $ do

  it "classifies the parts of a script" $ do
    let c = classOf project
    c "@config" 0 `shouldBe` Just StanzaHeader
    c "C" 0 `shouldBe` Just Parameter
    c "C" 1 `shouldBe` Just TypeName
    c "Z" 0 `shouldBe` Just TypeName
    c "0.75i" 0 `shouldBe` Just NumberLiteral
    c "\"C =\"" 0 `shouldBe` Just StringLiteral
    c "let" 0 `shouldBe` Just Keyword
    c "f" 0 `shouldBe` Just Function
    c "define" 0 `shouldBe` Just Keyword
    c "shade" 0 `shouldBe` Just CodeBlock
    c "shade" 1 `shouldBe` Just CodeBlock
    c "blend" 0 `shouldBe` Just BuiltinFunction
    c "light" 0 `shouldBe` Just BuiltinFunction
    c "blue" 0 `shouldBe` Just ColorName
    c "color" 0 `shouldBe` Just BuiltinValue
    c "iterate" 0 `shouldBe` Just Keyword
    c "->" 0 `shouldBe` Just Operator
    c "escapes" 0 `shouldBe` Just Keyword
    c "# the usual" 0 `shouldBe` Just Comment
    c "stuck" 0 `shouldBe` Just BuiltinValue
    c "iterations" 0 `shouldBe` Just BuiltinValue
    c "steps" 1 `shouldBe` Just Parameter
    c "run" 0 `shouldBe` Just Keyword

  it "knows each viewer's coordinate" $ do
    let c = classOf project
    c "z" 1 `shouldBe` Just Variable     -- in the body of `f`
    c "z" 3 `shouldBe` Just Variable     -- "Julia set"'s coordinate
    c "C" 4 `shouldBe` Just StanzaHeader -- `var=C`, in its header

  it "classifies handlers, tool fields, and their variables" $ do
    let c = classOf project
    c "on" 0 `shouldBe` Just Keyword
    c "click" 0 `shouldBe` Just Keyword
    c "drag" 0 `shouldBe` Just Keyword
    c "w" 0 `shouldBe` Just Variable
    c "erase" 0 `shouldBe` Just Keyword
    c "use" 0 `shouldBe` Just Keyword
    c "stroke" 0 `shouldBe` Just Keyword
    c "radius" 0 `shouldBe` Just Parameter   -- the tool's field
    c "radius" 2 `shouldBe` Just Parameter   -- `with radius radius`: the field
    c "p" 0 `shouldBe` Just Variable

  it "lets a tool see its viewer's coordinate" $ do
    let src = "@viewer \"V\" var=w\npass\n\n@tool \"T\"\non click:\n  draw point at w\n"
    classOf src "w" 1 `shouldBe` Just Variable

  it "doesn't take a block's colon for a declaration" $ do
    let src = "@viewer \"V\"\ncoloring:\n  period <- 1\nrepeat:\n  pass\nuntil true\n"
    classOf src "coloring" 0 `shouldBe` Just Keyword
    classOf src "period" 0 `shouldBe` Just Parameter
    classOf src "repeat" 0 `shouldBe` Just Keyword

  it "knows `stops` and `stopped`" $ do
    let src = "iterate z -> z^2 until z stops\nrepeat:\n  pass\nuntil z stopped\n"
    classOf src "stops" 0 `shouldBe` Just Keyword
    classOf src "stopped" 0 `shouldBe` Just Keyword

  it "keeps built-ins built-in, even where a script tries to define them" $ do
    let src = "define foo(bar):\n  bar <- bar + 1\n"
    classOf src "bar" 0 `shouldBe` Just BuiltinFunction
    classOf src "bar" 2 `shouldBe` Just BuiltinFunction

  it "lets a script name a variable with a statement word" $
    classOf project "line" 0 `shouldBe` Just Variable

  it "marks names that aren't defined" $
    classOf project "mystery" 0 `shouldBe` Just Undefined

  it "knows names defined elsewhere, if told" $ do
    let movie = "C <- 0.3 e^(2 pi i t)"
    [ h | (_, _, h) <- highlights movie ] `shouldSatisfy` (Undefined `elem`)
    let known = Map.fromList [("C", Parameter), ("t", Variable)]
    [ h | (_, _, h) <- highlightsWith known movie ] `shouldNotSatisfy` (Undefined `elem`)

  it "doesn't take a # in quotes for a comment" $
    classOf "write \"#1\" at z  # done" "\"#1\"" 0 `shouldBe` Just StringLiteral
