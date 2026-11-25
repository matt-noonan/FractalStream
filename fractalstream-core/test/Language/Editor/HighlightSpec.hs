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
C : C as "C =" := -0.12 + 0.75i
steps : Z := 20

@common
f(z) := z^2 + C
define shade(c):
  color <- blend(0.5, dark c, light c)

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
radius : R := 0.1
on click:
  w := click
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

  it "colors list and table types as types" $ do
    let src = "@config \"P\"\nxs : List of C := []\nT : Table 2 of R\n\n@viewer \"V\"\nk := 2\ncolor <- red\n"
        c = classOf src
    c "List" 0 `shouldBe` Just TypeName
    c "of" 0 `shouldBe` Just TypeName
    c "Table" 0 `shouldBe` Just TypeName
    c "2" 0 `shouldBe` Just TypeName
    c "of" 1 `shouldBe` Just TypeName
    c "R" 0 `shouldBe` Just TypeName
    -- (a number anywhere else is still a number)
    c "2" 1 `shouldBe` Just NumberLiteral

  it "colors a parenthesized type in a table's type as a type" $ do
    let src = "@config \"P\"\nC : C := 0\nT : Table 2 of (C x Z)\n\n@viewer \"V\"\ncolor <- red\n"
        c = classOf src
    c "C" 0 `shouldBe` Just Parameter
    c "C" 2 `shouldBe` Just TypeName
    c "x" 0 `shouldBe` Just TypeName
    c "Z" 0 `shouldBe` Just TypeName

  it "colors the words of table statements as keywords" $ do
    let src = unlines
          [ "@config \"P\"", "T : Table 3 of R", ""
          , "@viewer \"V\"", "color <- red", ""
          , "@tool \"Fill\" for=\"V\"", "on click:", "  p := click"
          , "  reset T to (viewer_width, viewer_height, 2) filled with 1"
          , "  resize T to (viewer_width, viewer_height, 3)"
          , "  T @ (point p, *) <- [1, 2, 3]" ]
        c = classOf src
    c "reset" 0 `shouldBe` Just Keyword
    c "resize" 0 `shouldBe` Just Keyword
    c "to" 0 `shouldBe` Just Keyword
    c "filled" 0 `shouldBe` Just Keyword
    c "point" 0 `shouldBe` Just Keyword

  it "classifies the parts of a script" $ do
    let c = classOf project
    c "@config" 0 `shouldBe` Just StanzaHeader
    c "C" 0 `shouldBe` Just Parameter
    c "C" 1 `shouldBe` Just TypeName
    c "Z" 0 `shouldBe` Just TypeName
    c "0.75i" 0 `shouldBe` Just NumberLiteral
    c "\"C =\"" 0 `shouldBe` Just StringLiteral
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
    c "click" 0 `shouldBe` Just Keyword        -- `on click:`
    c "click" 1 `shouldBe` Just BuiltinValue   -- `w := click`
    c "w" 0 `shouldBe` Just Variable
    c "erase" 0 `shouldBe` Just Keyword
    c "use" 0 `shouldBe` Just Keyword
    c "stroke" 0 `shouldBe` Just Keyword
    c "radius" 0 `shouldBe` Just Parameter   -- the tool's field
    c "radius" 2 `shouldBe` Just Parameter   -- `with radius radius`: the field
    c "p" 0 `shouldBe` Just Variable

  it "gives a tool its handlers' points, not its viewer's coordinate" $ do
    let src = "@viewer \"V\" var=w\npass\n\n@tool \"T\"\non click:\n  draw point at click\n  draw point at w\n"
    classOf src "click" 1 `shouldBe` Just BuiltinValue
    classOf src "w" 1 `shouldBe` Just Undefined

  it "knows a drag handler's `drag start` and `drag end`" $ do
    let src = "@viewer \"V\"\npass\n\n@tool \"T\"\non drag:\n  draw line from drag start to drag end\n"
    classOf src "drag" 0 `shouldBe` Just Keyword        -- `on drag:`
    classOf src "drag" 1 `shouldBe` Just BuiltinValue
    classOf src "start" 0 `shouldBe` Just BuiltinValue
    classOf src "end" 0 `shouldBe` Just BuiltinValue

  it "knows a real viewer's coordinates and pixel dimensions, in it and its tools" $ do
    let src = "@viewer \"V\" xvar=a yvar=b\ncolor <- if a < b and pixel_height > 0 then red else blue\n\n"
           ++ "@tool \"T\"\non click:\n  (a, b) := click\n  write text(pixel_width) at (a, b)\n"
    classOf src "a" 1 `shouldBe` Just Variable
    classOf src "b" 1 `shouldBe` Just Variable
    classOf src "pixel_height" 0 `shouldBe` Just Parameter
    classOf src "a" 2 `shouldBe` Just Variable
    classOf src "pixel_width" 0 `shouldBe` Just Parameter

  it "knows what a viewer's group or lattice gives its code" $ do
    let src = "@viewer \"V\" lattice=i\n(m, n) := cell\ncolor <- if side_distance < m then red else blue\n\n"
           ++ "@viewer \"G\" view=disk group=modular\ncolor <- if side_distance < fold_count + side_index then red else blue\n\n"
           ++ "@viewer \"P\"\ncolor <- if side_distance < 1 then red else blue\n"
    classOf src "cell" 0 `shouldBe` Just Variable
    classOf src "side_distance" 0 `shouldBe` Just Variable
    classOf src "side_distance" 1 `shouldBe` Just Variable
    classOf src "fold_count" 0 `shouldBe` Just Variable
    classOf src "side_index" 0 `shouldBe` Just Variable
    classOf src "side_distance" 2 `shouldBe` Just Undefined

  it "knows the variables of a tuple declaration" $ do
    let src = "@viewer \"V\"\nu, (v, w) := 1, (2, 3)\ncolor <- if u + v + w > 0 then red else blue\n"
    classOf src "u" 1 `shouldBe` Just Variable
    classOf src "v" 1 `shouldBe` Just Variable
    classOf src "w" 1 `shouldBe` Just Variable

  it "knows the points of the Riemann sphere: ∞, homogeneous coordinates, chart" $ do
    let src = "@viewer \"V\"\nw : Ĉ := infinity\n[a : b] := w\nq := [a^2 : b^2]\nx := affine(1, q) + chordal(w, ∞)\nc := chart(w)\n"
    classOf src "Ĉ" 0 `shouldBe` Just TypeName
    classOf src "infinity" 0 `shouldBe` Just BuiltinValue
    classOf src "∞" 0 `shouldBe` Just BuiltinValue
    classOf src "a" 1 `shouldBe` Just Variable
    classOf src "b" 1 `shouldBe` Just Variable
    classOf src "chart" 0 `shouldBe` Just BuiltinFunction
    classOf src "chordal" 0 `shouldBe` Just BuiltinFunction

  it "doesn't take a block's colon for a declaration" $ do
    let src = "@viewer \"V\"\ncoloring:\n  period <- 1\nrepeat:\n  pass\nuntil true\n"
    classOf src "coloring" 0 `shouldBe` Just Keyword
    classOf src "period" 0 `shouldBe` Just Parameter
    classOf src "repeat" 0 `shouldBe` Just Keyword

  it "knows `diff_bar` and the standard library's `℘`" $ do
    let src = "@viewer \"V\"\nd := diff_bar(|z|^2, z) + ℘'(z, 1, i)\ncolor <- domain_color(d)\n"
    classOf src "diff_bar" 0 `shouldBe` Just BuiltinFunction
    classOf src "℘" 0 `shouldBe` Just BuiltinFunction

  it "knows `stops` and `stopped`" $ do
    let src = "iterate z -> z^2 until z stops\nrepeat:\n  pass\nuntil z stopped\n"
    classOf src "stops" 0 `shouldBe` Just Keyword
    classOf src "stopped" 0 `shouldBe` Just Keyword

  it "knows `previous`" $ do
    let src = "repeat:\n  z <- z^2\n  draw line from previous z to z\nuntil z escapes\n"
    classOf src "previous" 0 `shouldBe` Just Keyword

  it "shows a built-in's name a script defines as the script's own, in that stanza" $ do
    let src = "define foo(bar):\n  bar <- bar + 1\n"
    classOf src "bar" 0 `shouldBe` Just Variable
    classOf src "bar" 2 `shouldBe` Just Variable
    let two = "@viewer \"V\"\nfall := 0.5\ncolor <- rainbow(fall)\n\n@viewer \"W\"\ncolor <- fall(0.5)\n"
    classOf two "fall" 1 `shouldBe` Just Variable
    classOf two "fall" 2 `shouldBe` Just BuiltinFunction

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
