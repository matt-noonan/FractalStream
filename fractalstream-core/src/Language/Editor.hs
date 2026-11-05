-- | Editing scripts, independently of any particular editor widget:
-- what a code editor needs to know about the language to help with it.
module Language.Editor
  ( indentUnit
  , newLineIndent
  , pasteIndented
  , toggleComments
  , deleteBackTo
  , visibleSpan
  , lastCharacterStart
  , firstCharacterLength
  , characterBoundary
  ) where

import Data.Char (isSpace, isMark, generalCategory, GeneralCategory(Format))
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Maybe (isJust)

import Language.Parser.Tokenizer (Token(..), SRToken(..), tokenize)
import Language.Parser.SourceRange (SourceRange(..), Pos(..))
import Language.Editor.Lines (commentStart)

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

-- | Where deleting the previous token goes back to, given the part of
-- the line before the cursor: past any spaces, to the start of the token
-- before them (@escapes@, @⟵@, @0.78i@, @"a string"@). In a comment, or a
-- string not finished yet, there are no tokens, so it's to the start of
-- the word. With only indentation before the cursor, it's the start of
-- the line.
deleteBackTo :: String -> Int
deleteBackTo before
  | null trimmed = 0
  | inProse = wordStart
  | otherwise = case reverse [ (t, s, e) | SRToken t (SourceRange (Pos _ s) (Pos _ e)) <- tokenize trimmed ] of
      -- an imaginary number, like 0.78i, is one thing to delete
      (I, s, _) : (n, s', e') : _ | isNumber n, e' + 1 == s -> s'
      (_, s, _) : _ -> s
      [] -> wordStart
  where
    isNumber = \case
      NumberI _ -> True
      NumberF _ -> True
      _ -> False
    trimmed = dropWhileEnd isSpace before
    wordStart = length (dropWhileEnd (not . isSpace) trimmed)
    inProse = isJust (commentStart trimmed) || inString trimmed
    -- whether the text ends inside a string: an odd number of quotes
    -- (not counting escaped ones)
    inString = go False
      where
        go q = \case
          [] -> q
          '\\' : _ : rest | q -> go q rest
          '"' : rest -> go (not q) rest
          _ : rest -> go q rest

-- | The part of a line to underline for the characters from @from@ up
-- to @to@ in it (character offsets): those characters, with the whole of
-- any character they're part of (a letter and the combining marks on
-- it), and more of the line if they'd show as nothing (combining marks
-- with nothing under them, or invisible formatting characters). Nothing,
-- if nothing in the line shows. (An editor can't draw an underline
-- that's no wider than nothing.)
visibleSpan :: String -> Int -> Int -> Maybe (Int, Int)
visibleSpan line from to = grow (whole (clamp from) (clamp (max to (from + 1))))
  where
    n = length line
    clamp = max 0 . min n
    at i = line !! i
    invisible c = isMark c || generalCategory c == Format
    -- back to the character the marks are on, and on over its marks
    whole i j = (back i, forward j)
    back i = if i > 0 && i < n && isMark (at i) then back (i - 1) else i
    forward j = if j < n && isMark (at j) then forward (j + 1) else j
    showsUp (i, j) = any (not . invisible) (take (j - i) (drop i line))
    grow (i, j)
      | i < j, showsUp (i, j) = Just (i, j)
      | j < n = grow (i, forward (j + 1))
      | i > 0 = grow (back (i - 1), j)
      | otherwise = Nothing

-- | Where the last whole character of some text starts (a character
-- offset): its letter, with the combining marks after it (so that
-- deleting back deletes @x̂@ all at once, as it does a composed @â@).
lastCharacterStart :: String -> Int
lastCharacterStart s =
  let n = length s
      marks = length (takeWhile isMark (reverse s))
  in n - marks - (if marks < n then 1 else 0)

-- | How long the first whole character of some text is (in
-- characters): its letter, and the combining marks after it.
firstCharacterLength :: String -> Int
firstCharacterLength = \case
  [] -> 0
  _ : rest -> 1 + length (takeWhile isMark rest)

-- | Where the cursor goes, if it's at character offset @i@ of a line,
-- so that it isn't inside a whole character (between a letter and its
-- combining marks, or between marks): to the start of the character, if
-- @back@, or else to its end. Anywhere else, it stays.
characterBoundary :: String -> Int -> Bool -> Int
characterBoundary line i back
  | i > 0, i < length line, isMark (line !! i) =
      if back then lastCharacterStart (take i line)
      else i + length (takeWhile isMark (drop i line))
  | otherwise = i
