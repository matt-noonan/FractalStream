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
-- A user's own projects are browsed the same way, from a folder laid out
-- the same way ('myProjectsDirectory'), but read when asked for
-- ('scanProjectsIn') rather than compiled in.
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
  , myProjectsDirectory
  , xdgDocumentsDirectory
  , scanProjectsIn
  , placeholderPreview
  ) where

import Actor.Ensemble (readProjectMetadata, checkProjectBytes)
import Actor.Metadata (ProjectMetadata(..))

import Control.Exception (IOException, try)
import Control.Monad (filterM, forM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.FileEmbed (embedDir, embedFile)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (groupBy, isPrefixOf, sort, sortOn, stripPrefix)
import Data.Maybe (fromMaybe)
import System.Directory (XdgDirectory(..), doesDirectoryExist, doesFileExist, getHomeDirectory,
                         getUserDocumentsDirectory, getXdgDirectory, listDirectory)
import System.Info (os)

import System.FilePath ((</>), splitDirectories, takeBaseName, takeExtension)

data LibrarySection = LibrarySection
  { lsName    :: String
  , lsStartingPoints :: Bool
    -- ^ Whether this is the "Starting points" section, whose projects
    -- are for starting a new project from: opening one should give a
    -- new, untitled project to edit, not a copy of a finished one.
  , lsMine    :: Bool
    -- ^ Whether this is one of the user's own sections, from their
    -- projects folder ('myProjectsDirectory'), rather than part of the
    -- built-in library.
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
  , leFile        :: Maybe FilePath
    -- ^ Where the project is, for one of the user's own projects, which
    -- opens in place rather than as a copy.
  , leProblem     :: Maybe String
    -- ^ Why the file couldn't be read as a project, if it couldn't.
  }

embeddedLibrary :: [(FilePath, ByteString)]
embeddedLibrary = $(embedDir "projects")

-- | What to show for a project without a preview of its own: a blank
-- page (a PNG image, with a transparent background).
placeholderPreview :: ByteString
placeholderPreview = $(embedFile "assets/no-preview.png")

librarySections :: [LibrarySection]
librarySections =
  [ LibrarySection name isStart False (sortOn leTitle (map (uncurry (libraryEntry isStart Nothing)) (map snd files)))
  | files@((dir, _) : _) <- groupBy ((==) `on` fst) (sortOn fst projects)
  , let name = sectionName dir
        isStart = name == "Starting points" ]
  where
    projects = [ (dir, (name, contents))
               | (path, contents) <- embeddedLibrary
               , [dir, name] <- [splitDirectories path]
               , takeExtension name == ".fs" ]

-- | Where a user's own projects go: a @FractalStream@ folder in their
-- documents folder (which needn't exist yet).
myProjectsDirectory :: IO FilePath
myProjectsDirectory = (</> "FractalStream") <$> documentsDirectory

-- | The user's documents folder. 'getUserDocumentsDirectory' only finds
-- it on Windows: everywhere else, it gives the home folder itself. So on
-- macOS it's @~/Documents@, and on Linux (and the like) it's whatever the
-- desktop's own settings say (@XDG_DOCUMENTS_DIR@, in @user-dirs.dirs@,
-- which can be a translated name), or else @~/Documents@.
documentsDirectory :: IO FilePath
documentsDirectory = case os of
  "mingw32" -> getUserDocumentsDirectory
  "darwin"  -> (</> "Documents") <$> getHomeDirectory
  _ -> do
    home <- getHomeDirectory
    settings <- getXdgDirectory XdgConfig "user-dirs.dirs"
    configured <- try (readFile settings) <&> \case
      Left (_ :: IOException) -> Nothing
      Right contents -> xdgDocumentsDirectory home contents
    pure (fromMaybe (home </> "Documents") configured)

-- | The documents folder a @user-dirs.dirs@ file names, given the home
-- folder (which its paths are written relative to, as @$HOME/...@).
xdgDocumentsDirectory :: FilePath -> String -> Maybe FilePath
xdgDocumentsDirectory home contents = case
    [ value | line <- lines contents
            , Just value <- [stripPrefix "XDG_DOCUMENTS_DIR=" (dropWhile (== ' ') line)] ] of
  value : _ -> case unquote value of
    "$HOME" -> Nothing  -- the home folder itself: not a documents folder
    unquoted | Just rest <- stripPrefix "$HOME/" unquoted -> Just (home </> rest)
             | "/" `isPrefixOf` unquoted -> Just unquoted
             | otherwise -> Nothing
  [] -> Nothing
  where
    unquote s = case s of
      '"' : rest -> takeWhile (/= '"') rest
      _ -> s

-- | Read the user's projects from a folder laid out like the built-in
-- library: the projects directly in it make up a section called "My
-- projects", which always comes first (even when it's empty, or the
-- folder doesn't exist), and each subfolder is a section of its own.
-- Projects without metadata are titled by their filename; a file that
-- can't be read as a project, or whose script has an error (checked
-- without building it, by 'checkProjectBytes'), is listed anyway, with
-- what went wrong.
scanProjectsIn :: FilePath -> IO [LibrarySection]
scanProjectsIn dir = doesDirectoryExist dir >>= \case
  False -> pure [mine "My projects" []]
  True -> do
    names <- visible <$> listDirectory dir
    root <- projectsIn dir names
    subdirs <- filterM (doesDirectoryExist . (dir </>)) names
    subsections <- forM subdirs $ \sub ->
      mine (sectionName sub) <$> (projectsIn (dir </> sub) . visible =<< listDirectory (dir </> sub))
    pure (mine "My projects" root : subsections)
  where
    mine name = LibrarySection name False True
    visible = sort . filter (not . ("." `isPrefixOf`))
    projectsIn folder names = do
      files <- filterM (\n -> (takeExtension n == ".fs" &&) <$> doesFileExist (folder </> n)) names
      sortOn leTitle <$> forM files (\name -> do
        let path = folder </> name
        try (BS.readFile path) <&> \case
          Left (e :: IOException) -> (libraryEntry False (Just path) name BS.empty)
                                       { leProblem = Just ("This file couldn't be read: " ++ show e) }
          Right contents ->
            let entry = libraryEntry False (Just path) name contents
            in case (leProblem entry, checkProjectBytes contents) of
                 (Nothing, Left err) -> entry
                   { leProblem = Just ("This project has an error in its script:\n\n" ++ err) }
                 _ -> entry)


-- | A project's entry, from its filename and contents.
libraryEntry :: Bool -> Maybe FilePath -> FilePath -> ByteString -> LibraryEntry
libraryEntry isStart file name contents = LibraryEntry
  { leFileName = name
  , leTitle = fromMaybe (takeBaseName name) (md >>= mdTitle)
  , leDescription = fromMaybe "" (md >>= mdDescription)
  , lePreview = md >>= mdPreview
  , leContents = contents
  , leStartingPoint = isStart
  , leFile = file
  , leProblem = either (\err -> Just ("This file couldn't be read as a FractalStream project: it " ++ err ++ "."))
                       (const Nothing) parsed
  }
  where
    parsed = readProjectMetadata contents
    md = either (const Nothing) Just parsed

-- | A section's name, from its folder's: without any leading number
-- (and the space after it) that's there to put the sections in order.
sectionName :: FilePath -> String
sectionName dir = case span isDigit dir of
  (_ : _, ' ' : rest) -> rest
  _ -> dir
