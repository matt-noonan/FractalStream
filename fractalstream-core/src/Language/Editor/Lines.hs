-- | A script's lines as an editor sees them: each with its tokens, as
-- character offsets into the script, and where its comment starts.
-- Shared by "Language.Editor.Highlight" and "Language.Editor.Normalize".
module Language.Editor.Lines
  ( Line(..)
  , scriptLines
  , isStanzaHeader
  ) where

import Data.List (isPrefixOf)

import Language.Parser.Tokenizer (Token(..), SRToken(..), tokenize)
import Language.Parser.SourceRange (SourceRange(..), Pos(..))

-- | A line of the script: where it starts, its tokens (before any
-- comment), where its comment starts (if it has one), and its
-- indentation.
data Line = Line
  { lnText    :: String
  , lnStart   :: Int
  , lnTokens  :: [(Int, Int, Token)]
  , lnComment :: Maybe Int
  , lnIndent  :: Int
  }

scriptLines :: String -> [Line]
scriptLines src = go 0 (splitLines src)
  where
    go _ [] = []
    go off (l : rest) = mkLine off l : go (off + length l + 1) rest
    mkLine off l =
      let commentAt = commentStart l
          code = maybe l (`take` l) commentAt
          toks = [ (off + s, off + e + 1, t)
                 | SRToken t (SourceRange (Pos _ s) (Pos _ e)) <- tokenize code ]
      in Line l off toks ((off +) <$> commentAt) (length (takeWhile (== ' ') l))
    splitLines s = case break (== '\n') s of
      (l, []) -> [l]
      (l, _ : more) -> l : splitLines more

-- | Where a line's comment starts: its first @#@ that isn't in quotes.
commentStart :: String -> Maybe Int
commentStart = go 0 False
  where
    go _ _ [] = Nothing
    go i quoted (c : cs) = case c of
      '#' | not quoted -> Just i
      '\\' | quoted, _ : more <- cs -> go (i + 2) quoted more
      '"' -> go (i + 1) (not quoted) cs
      _ -> go (i + 1) quoted cs

-- | Whether a line is a stanza's header (@\@viewer ...@).
isStanzaHeader :: Line -> Bool
isStanzaHeader l = "@" `isPrefixOf` lnText l
