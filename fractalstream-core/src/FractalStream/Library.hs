{-# language TemplateHaskell #-}
{-# options_ghc -fforce-recomp #-}

-- | The library of projects that ships with FractalStream: every
-- project under the repository's top-level @library/@ directory,
-- compiled right into the app (so there are no files to find at run
-- time, wherever and however the app was installed).
--
-- Each subdirectory of @library/@ is a section. A section's directory
-- name can start with a number and a space, to put the sections in
-- order (@1 Getting started@, @2 Quadratic family@, ...); the number
-- isn't part of the name shown. Within a section, projects are in
-- order of title.
--
-- This lives in the core rather than in a frontend because embedding
-- runs Template Haskell, which has GHC load the module's dependencies
-- at compile time -- and loading the LLVM backend's static libraries
-- that way fails. (The core's other embedded files are here for the
-- same reason.)
--
-- Note: neither Stack nor GHC notices a project being /added to/ or
-- /removed from/ @library/@ (it's outside this package, and GHC only
-- tracks the files this module already embedded), so after adding or
-- removing one, run @stack build --force-dirty@ here; the
-- @-fforce-recomp@ above then makes sure this module is rebuilt. (The
-- core's LibrarySpec fails if the embedded library is out of date.)
module FractalStream.Library
  ( LibrarySection(..)
  , LibraryEntry(..)
  , librarySections
  ) where

import Actor.Ensemble (readProjectMetadata)
import Actor.Metadata (ProjectMetadata(..))

import Data.ByteString (ByteString)
import Data.Char (isDigit)
import Data.FileEmbed (embedDir)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Maybe (fromMaybe)
import System.FilePath (splitDirectories, takeBaseName, takeExtension)

data LibrarySection = LibrarySection
  { lsName    :: String
  , lsEntries :: [LibraryEntry]
  }

data LibraryEntry = LibraryEntry
  { leFileName    :: FilePath
    -- ^ The project's own filename, e.g. @julia-set.fs@: what to
    -- suggest when a copy of it is first saved.
  , leTitle       :: String
  , leDescription :: String
  , lePreview     :: Maybe ByteString
    -- ^ A PNG image.
  , leContents    :: ByteString
    -- ^ The whole project file.
  }

embeddedLibrary :: [(FilePath, ByteString)]
embeddedLibrary = $(embedDir "../library")

librarySections :: [LibrarySection]
librarySections =
  [ LibrarySection (sectionName dir) (sortOn leTitle (map (entry . snd) files))
  | files@((dir, _) : _) <- groupBy ((==) `on` fst) (sortOn fst projects) ]
  where
    projects = [ (dir, (name, contents))
               | (path, contents) <- embeddedLibrary
               , [dir, name] <- [splitDirectories path]
               , takeExtension name == ".fs" ]
    entry (name, contents) =
      -- Every library project is checked to have its metadata (see the
      -- core's LibrarySpec), so falling back to the filename here is
      -- only ever a safety net.
      let md = either (const Nothing) Just (readProjectMetadata contents)
      in LibraryEntry
           { leFileName = name
           , leTitle = fromMaybe (takeBaseName name) (md >>= mdTitle)
           , leDescription = fromMaybe "" (md >>= mdDescription)
           , lePreview = md >>= mdPreview
           , leContents = contents
           }
    sectionName dir = case span isDigit dir of
      (_ : _, ' ' : rest) -> rest
      _ -> dir
