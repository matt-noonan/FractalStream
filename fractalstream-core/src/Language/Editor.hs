-- | Editing scripts, independently of any particular editor widget:
-- what a code editor needs to know about the language to help with it.
module Language.Editor
  ( indentUnit
  , newLineIndent
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

-- | How many spaces one level of indentation is.
indentUnit :: Int
indentUnit = 2

-- | How far to indent a new line started by breaking a line, given the
-- part of the line before the break: as far as that line is indented,
-- and one level further if it opens a block (ends with @:@, not counting
-- a comment after it).
newLineIndent :: String -> Int
newLineIndent before =
  let indent = length (takeWhile (== ' ') before)
      code = dropWhileEnd isSpace (withoutComment before)
  in if not (null code) && last code == ':' then indent + indentUnit else indent

-- | A line without its comment: from a @#@ that isn't in quotes on.
withoutComment :: String -> String
withoutComment = go False
  where
    go _ [] = []
    go inQuotes (c : cs) = case c of
      '#' | not inQuotes -> []
      '\\' | inQuotes, (d : ds) <- cs -> c : d : go inQuotes ds
      '"' -> c : go (not inQuotes) cs
      _ -> c : go inQuotes cs
