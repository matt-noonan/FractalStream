-- | Editing scripts, independently of any particular editor widget:
-- what a code editor needs to know about the language to help with it.
module Language.Editor
  ( indentUnit
  , newLineIndent
  , pasteIndented
  , toggleComments
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)

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

-- | Text pasted where the line so far is @before@, re-indented to fit
-- there. Text of one line is pasted as it is. Otherwise its first line
-- goes where it's pasted, and the lines after it keep their indentation
-- relative to it: so a block copied from one level and pasted at
-- another moves to that level as a whole.
--
-- Copied text often starts partway into its first line (at the code, not
-- the indentation before it), so how far in its first line was has to
-- be inferred: as far as the next line, or a level less if the first
-- line opens a block.
pasteIndented :: String -> String -> String
pasteIndented before pasted
  | '\n' `notElem` text = text
  | otherwise = case lines' text of
      [] -> text
      first : rest ->
        let atIndentation = all (== ' ') before
            target = if atIndentation then length before else indentOf before
            baseline = case filter (not . blank) rest of
              next : _ | indentOf first == 0 ->
                let opens = newLineIndent first > 0
                in max 0 (indentOf next - (if opens then indentUnit else 0))
              _ -> indentOf first
            first' = if atIndentation then dropWhile (== ' ') first else first
            shift l | blank l = ""
                    | otherwise = replicate (max 0 (target + indentOf l - baseline)) ' '
                                  ++ dropWhile (== ' ') l
        in unlinesKeepingEnd (first' : map shift rest)
  where
    text = filter (/= '\r') pasted
    indentOf = length . takeWhile (== ' ')
    blank = all isSpace
    -- (unlike 'lines', a final newline leaves a last, empty line, so
    -- putting the lines back together gives back the final newline)
    lines' s = case break (== '\n') s of
      (l, []) -> [l]
      (l, _ : more) -> l : lines' more
    unlinesKeepingEnd = foldr1 (\l acc -> l ++ "\n" ++ acc)

-- | Comment out lines, or, if they're all comments already, uncomment
-- them. Comment marks go at the lines' shallowest indentation, so they
-- line up; blank lines are left alone. A single blank line gets a
-- comment mark at its indentation, to start writing a comment there.
toggleComments :: [String] -> [String]
toggleComments ls
  | [l] <- ls, blank l = [l ++ "# "]
  | all blank ls = ls
  | all (\l -> blank l || isComment l) ls = map uncomment ls
  | otherwise = map comment ls
  where
    blank = all isSpace
    isComment l = "#" `isPrefixOf` dropWhile (== ' ') l
    column = minimum [ length (takeWhile (== ' ') l) | l <- ls, not (blank l) ]
    comment l
      | blank l = l
      | otherwise = let (indent, code) = splitAt column l in indent ++ "# " ++ code
    uncomment l =
      let (indent, rest) = span (== ' ') l
      in case rest of
           '#' : ' ' : code -> indent ++ code
           '#' : code -> indent ++ code
           _ -> l
