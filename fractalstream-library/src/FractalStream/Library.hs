{-# language TemplateHaskell #-}
{-# options_ghc -fforce-recomp #-}

-- | The library of projects that ships with FractalStream: every
-- project under this package's @projects/@ directory,
-- compiled right into the app (so there are no files to find at run
-- time, wherever and however the app was installed).
--
-- Each subdirectory of @projects/@ is a section. A section's directory
-- name can start with a number and a space, to put the sections in
-- order (@1 Getting started@, @2 Quadratic family@, ...); the number
-- isn't part of the name shown. Within a section, projects are in
-- order of title.
--
-- The section called "Starting points" is special: its projects are
-- templates for new projects, rather than finished ones to explore (see
-- 'lsStartingPoints').
--
-- This is a package of its own, rather than part of a frontend, for two
-- reasons. Embedding runs Template Haskell, which has GHC load the
-- module's dependencies while compiling it, and loading the LLVM
-- backend's static libraries that way fails; and a frontend that has no
-- use for the library (like fsrun) needn't carry it.
--
-- The package's cabal file lists the projects as extra source files, so
-- Stack rebuilds the package when one is added, removed, or changed; and
-- @-fforce-recomp@ makes GHC rebuild this module when it does (GHC
-- itself only notices changes to the files it already embedded).
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
  , lsStartingPoints :: Bool
    -- ^ Whether this is the "Starting points" section, whose projects
    -- are for starting a new project from: opening one should give a
    -- new, untitled project to edit, not a copy of a finished one.
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
  , leStartingPoint :: Bool
    -- ^ Whether it's in the "Starting points" section.
  }

embeddedLibrary :: [(FilePath, ByteString)]
embeddedLibrary = $(embedDir "projects")

librarySections :: [LibrarySection]
librarySections =
  [ LibrarySection name isStart (sortOn leTitle (map (entry isStart . snd) files))
  | files@((dir, _) : _) <- groupBy ((==) `on` fst) (sortOn fst projects)
  , let name = sectionName dir
        isStart = name == "Starting points" ]
  where
    projects = [ (dir, (name, contents))
               | (path, contents) <- embeddedLibrary
               , [dir, name] <- [splitDirectories path]
               , takeExtension name == ".fs" ]
    entry isStart (name, contents) =
      -- Every library project is checked to have its metadata (see the
      -- LibrarySpec), so falling back to the filename here is
      -- only ever a safety net.
      let md = either (const Nothing) Just (readProjectMetadata contents)
      in LibraryEntry
           { leFileName = name
           , leTitle = fromMaybe (takeBaseName name) (md >>= mdTitle)
           , leDescription = fromMaybe "" (md >>= mdDescription)
           , lePreview = md >>= mdPreview
           , leContents = contents
           , leStartingPoint = isStart
           }

    sectionName dir = case span isDigit dir of
      (_ : _, ' ' : rest) -> rest
      _ -> dir
