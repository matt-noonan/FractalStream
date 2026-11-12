{-# LANGUAGE OverloadedStrings #-}
{- |
Module       : Editor
Description  : What the page's script editor asks the engine

The script editor (@web/src/editor.ts@) colors a script, marks its error,
and rewrites it with mathematical symbols, as the desktop app's does, from
the core's "Language.Editor" modules and 'checkScriptWithColorizers'.

Offsets are in UTF-16 code units, as JavaScript counts them, not in
characters as the core does: a character outside the Basic Multilingual
Plane (like 𝑖) is two of them.

> { "type": "analysis", "id", "spans": [ [from, to, class] ], "error": { "from", "to", "message" } or null,
>   "normalizations": [ [from, to, text] ] }
> { "type": "symbols", "names": [ [name, symbol] ], "accents": [ [name, mark] ] }

where a class is a 'highlightName' (@"keyword"@, @"parameter"@, ...).
-}
module Editor
  ( analysisJSON
  , symbolsJSON
  , errorJSON
  ) where

import Actor.Ensemble (checkScriptWithColorizers)
import Language.Editor.Highlight (highlights, highlightName)
import Language.Editor.Normalize (normalizations)
import Language.Editor.Symbols (symbolNames, accentNames)
import Language.Parser.SourceRange (SourceRange(..), Pos(..))

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Array (listArray, (!), bounds)
import qualified Data.Map as Map

-- | A script's colors, its error (if it doesn't check out), and how to
-- write it with mathematical symbols, for the editor's request @id@.
analysisJSON :: Int -> Map.Map String String -> String -> Aeson.Value
analysisJSON reqId choices src = Aeson.object
  [ "type" .= ("analysis" :: String)
  , "id" .= reqId
  , "spans" .= [ (u16 from, u16 to, highlightName h) | (from, to, h) <- highlights src ]
  , "error" .= either (Just . errorJSON src) (const Nothing) (checkScriptWithColorizers choices src)
  , "normalizations" .= [ (u16 from, u16 to, text) | (from, to, text) <- normalizations src ]
  ]
  where u16 = utf16Offset src

-- | An error's place in a script (in UTF-16 offsets, the end not
-- included: the whole of its rows, if it spans several), and its message.
errorJSON :: String -> (SourceRange, String) -> Aeson.Value
errorJSON src (range, message) = Aeson.object $ ("message" .= message) : case range of
  NoSourceRange -> [ "from" .= Aeson.Null, "to" .= Aeson.Null ]
  -- (rows and columns count from 0, and the last column is included, as
  -- 'Language.Parser.SourceRange.indicateError' shows them)
  SourceRange (Pos r c) (Pos r' c')
    | r == r' -> [ "from" .= u16 (lineStart r + c), "to" .= u16 (lineStart r + c' + 1) ]
    | otherwise -> [ "from" .= u16 (lineStart r), "to" .= u16 (lineStart (r' + 1)) ]
  where
    u16 = utf16Offset src . min (length src) . max 0
    -- (a row past the end starts at the end)
    starts = 0 : [ i + 1 | (i, '\n') <- zip [0 ..] src ]
    lineStart k = case drop k starts of
      s : _ | k >= 0 -> s
      _ -> length src

-- | The symbols' names, and the accents', for the editor's symbol entry.
symbolsJSON :: Aeson.Value
symbolsJSON = Aeson.object
  [ "type" .= ("symbols" :: String)
  , "names" .= symbolNames
  , "accents" .= [ (name, [c]) | (name, c) <- accentNames ]
  ]

-- | A character offset into a string, as a UTF-16 offset.
utf16Offset :: String -> Int -> Int
utf16Offset src = \i -> offsets ! max 0 (min i (snd (bounds offsets)))
  where
    offsets = listArray (0, length src) (scanl (\n c -> n + if fromEnum c > 0xFFFF then 2 else 1) 0 src)
