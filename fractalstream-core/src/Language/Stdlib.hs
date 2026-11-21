{-# language TemplateHaskell #-}
{-# options_ghc -fforce-recomp #-}

-- | FractalStream's standard library: definitions, written in the script
-- language itself (in @stdlib/standard.fs@, compiled into the app), that
-- every project has, as if they were in its @\@common@ stanza.
--
-- A project can define any of the standard library's names for itself
-- (see 'Language.Parser.ownBuiltinNames'), and where it does, its own
-- definition is the one it gets. So adding a definition here never
-- changes what an existing project means.
--
-- Scripts reach the standard library's definitions through names they
-- can't write ('stdlibName'): the grammar reads a standard library name
-- that the script doesn't define for itself as the standard library's
-- (see "Language.Value.Parser"), and the standard library's own code is
-- read the same way. So its definitions always use each other, never a
-- project's definitions of the same names.
--
-- This module only knows the standard library's source and the names it
-- defines (from its tokens), so that the parsers can depend on it; the
-- parsed definitions are 'Language.Value.Parser.stdlibDefinitions'.
--
-- (@-fforce-recomp@: GHC only notices a change to an embedded file it
-- has already embedded, so this module is always rebuilt.)
module Language.Stdlib
  ( stdlibSource
  , stdlibFunctionNames
  , isStdlibFunction
  , stdlibName
  , stdlibPrefix
  , fromStdlibName
  , withoutStdlibPrefix
  ) where

import FractalStream.Prelude

import Data.FileEmbed (embedFile)
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Language.Parser.Tokenizer

-- | The standard library's source: a sequence of definitions, as in a
-- project's @\@common@ stanza.
stdlibSource :: String
stdlibSource = Text.unpack (Text.decodeUtf8 stdlibBytes)

stdlibBytes :: BS.ByteString
stdlibBytes = $(embedFile "stdlib/standard.fs")

-- | The names of the functions the standard library defines, as scripts
-- write them.
stdlibFunctionNames :: Set String
stdlibFunctionNames =
  Set.fromList (definedFunctionNames (map baseToken (tokenizeWithIndentation stdlibSource)))

isStdlibFunction :: String -> Bool
isStdlibFunction n = Set.member n stdlibFunctionNames

-- | The name the standard library's definition of a name goes by, which
-- no script can write.
stdlibName :: String -> String
stdlibName = (stdlibPrefix ++)

stdlibPrefix :: String
stdlibPrefix = "[std] "

-- | The name as a script writes it, if this is a standard library
-- definition's name (for messages).
fromStdlibName :: String -> Maybe String
fromStdlibName n = case splitAt (length stdlibPrefix) n of
  (pre, rest) | pre == stdlibPrefix -> Just rest
  _ -> Nothing

-- | A message with the standard library's names in it as scripts write
-- them.
withoutStdlibPrefix :: String -> String
withoutStdlibPrefix = \case
  [] -> []
  s@(c : rest) -> case fromStdlibName s of
    Just after -> withoutStdlibPrefix after
    Nothing -> c : withoutStdlibPrefix rest
