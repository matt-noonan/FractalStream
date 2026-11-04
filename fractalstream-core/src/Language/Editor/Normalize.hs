-- | Rewriting a script to use the mathematical symbols the language
-- accepts in place of their ASCII spellings: @x : ℝ ⟵ 1@ for
-- @x : R <- 1@, @iterate z ⟼ z² + C@ for @iterate z -> z^2 + C@, and so
-- on. Only spellings change: the rewritten script has exactly the same
-- tokens. Stanza headers, comments, and strings are left alone.
module Language.Editor.Normalize
  ( normalizations
  , normalize
  ) where

import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Ord (Down(..))

import Language.Parser.Tokenizer (Token(..))
import Language.Editor.Lines

-- | The rewrites to make, as character ranges of the script (from, up to
-- but not including, to) and their replacements, in order.
normalizations :: String -> [(Int, Int, String)]
normalizations src = concatMap lineEdits (scriptLines src)
  where
    lineEdits l
      | isStanzaHeader l = []
      | otherwise = go Nothing False (lnTokens l)

    textOf (from, to, _) = take (to - from) (drop from src)

    -- the token before, and whether it was a type, for spotting types
    go _ _ [] = []
    go before afterType toks = case toks of
      -- an integer power: z^2, z^(2), z^(-1)
      c@(from, _, Caret) : n@(_, to, NumberI k) : rest
        | textOf c == "^", all isDigit (textOf n) ->
            (from, to, superscript k) : go (Just (NumberI k)) False rest
      c@(from, _, Caret) : (_, _, OpenParen) : n@(_, _, NumberI k) : (_, to, CloseParen) : rest
        | textOf c == "^", all isDigit (textOf n) ->
            (from, to, superscript k) : go (Just CloseParen) False rest
      c@(from, _, Caret) : (_, _, OpenParen) : (_, _, Minus) : n@(_, _, NumberI k) : (_, to, CloseParen) : rest
        | textOf c == "^", all isDigit (textOf n) ->
            (from, to, '⁻' : superscript k) : go (Just CloseParen) False rest
      t@(from, to, tok) : rest ->
        let isType = case tok of
              Identifier "x" | afterType -> True   -- a pair type's `x` goes on with it
              Identifier n -> inTypePosition before afterType && n `elem` typeNames
              _ -> False
        in [ (from, to, new) | Just new <- [respell t isType] ] ++ go (Just tok) isType rest

    respell t@(_, _, tok) isType = case tok of
      Identifier n | isType -> lookup n typeSymbols
      Pi | textOf t == "pi" -> Just "π"
      _ -> lookup (textOf t) operatorSymbols

    -- what a type comes after: `x : R`, `List of C`, and the `x` of a
    -- pair type, `R x R`, after another type
    inTypePosition before afterType = case before of
      Just Colon -> True
      Just (Identifier "of") -> True
      Just (Identifier "x") -> afterType
      _ -> False
    -- (`List` and `of` count as types, so that `List of C x R` works)
    typeNames = map fst typeSymbols ++ ["ℝ", "ℂ", "ℤ", "List", "Boolean", "Bool", "Color", "Text"]

-- | Apply all of 'normalizations'.
normalize :: String -> String
normalize src = foldl apply src (sortOn (\(from, _, _) -> Down from) (normalizations src))
  where
    apply s (from, to, new) = take from s ++ new ++ drop to s

operatorSymbols :: [(String, String)]
operatorSymbols =
  [ ("<-", "⟵"), ("->", "⟼"), (":=", "≔"), ("<=", "≤"), (">=", "≥")
  , ("!=", "≠"), ("=/=", "≠"), ("===", "≡"), ("==", "="), ("'", "′") ]

typeSymbols :: [(String, String)]
typeSymbols =
  [ ("R", "ℝ"), ("Real", "ℝ"), ("C", "ℂ"), ("Complex", "ℂ")
  , ("Z", "ℤ"), ("Int", "ℤ"), ("Integer", "ℤ") ]

superscript :: Integer -> String
superscript = map digit . show
  where
    digit c = case c of
      '0' -> '⁰'; '1' -> '¹'; '2' -> '²'; '3' -> '³'; '4' -> '⁴'
      '5' -> '⁵'; '6' -> '⁶'; '7' -> '⁷'; '8' -> '⁸'; _ -> '⁹'
