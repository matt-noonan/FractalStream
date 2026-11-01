{-# language OverloadedStrings #-}

-- | Checks on the library of projects that ships with FractalStream
-- (this package's @projects/@ directory, one subdirectory per section):
-- every one must open, and must have everything the library browser
-- shows -- a title, a description, and a preview -- along with the
-- preview commands that remake its preview (@fsrun --update-preview@),
-- unless its preview is drawn by hand ('handMadePreviews').
--
-- A library project may remember where its windows go (in
-- @wx-properties:@), but not a current view (a viewer's @center@ or
-- @pixel-size@ there): opening it from the library should start at the
-- view its script describes. Saving a project from the app records the
-- current view, so this catches one saved back into the library as is.
module LibrarySpec (spec) where

import Test.Hspec

import Actor.Ensemble
import Actor.Metadata (ProjectMetadata(..))
import FractalStream.Library
import Data.DynamicValue

import Control.Monad (filterM, forM_)
import Data.Aeson (Value(..))
import Data.String (fromString)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (isSuffixOf, sort)
import Data.Maybe (isJust)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeBaseName)
import Data.Char (isDigit)

libraryDir :: FilePath
libraryDir = "projects"

-- | The projects whose preview is drawn by hand (an icon, rather than a
-- picture of the project), and so has no preview commands to remake it.
handMadePreviews :: [FilePath]
handMadePreviews = [ "0 Starting points" </> "blank-project.fs" ]


spec :: Spec
spec = describe "the built-in project library" $ do
  sections <- runIO $
    listDirectory libraryDir >>= filterM (doesDirectoryExist . (libraryDir </>)) . sort
  it "has at least one section" $
    sections `shouldNotBe` []

  it "is all embedded in the app, sections in order and named without \
     \their ordering numbers, each project titled by its metadata" $ do
    let embedded = [ (lsName s, length (lsEntries s)) | s <- librarySections ]
    onDisk <- mapM (\s -> (,) s . length . filter (".fs" `isSuffixOf`)
                            <$> listDirectory (libraryDir </> s)) sections
    map snd embedded `shouldBe` map snd onDisk
    map fst embedded `shouldBe` map (dropOrdering . fst) onDisk
    [ leTitle e | s <- librarySections, e <- lsEntries s, leTitle e == takeBaseName (leFileName e) ]
      `shouldBe` []

  forM_ sections $ \section -> do
    projects <- runIO $ sort . filter (".fs" `isSuffixOf`) <$> listDirectory (libraryDir </> section)
    describe section $ do
      it "has at least one project" $
        projects `shouldNotBe` []
      forM_ projects $ \project ->
        it project $ parseEnsembleFromFile (libraryDir </> section </> project) >>= \case
          Left err -> expectationFailure err
          Right ens -> do
            md <- getDynamic (ensembleMetadata ens)
            (isJust (mdTitle md), isJust (mdDescription md))
              `shouldBe` (True, True)
            isJust (mdPreview md) `shouldBe` True
            isJust (mdPreviewCommands md)
              `shouldBe` ((section </> project) `notElem` handMadePreviews)
            props <- getDynamic (ensembleFrontendProperties ens)
            savedViews props `shouldBe` []

-- | Every viewer (by title) whose @wx-properties:@ entry records a
-- current view, with the keys that do.
savedViews :: KM.KeyMap Value -> [(String, [String])]
savedViews props = case KM.lookup "wx-properties" props of
  Just (Object wx) | Just (Object viewers) <- KM.lookup "viewers" wx ->
    [ (Key.toString title, viewKeys)
    | (title, Object viewer) <- KM.toList viewers
    , let viewKeys = [ k | k <- ["center", "pixel-size"], KM.member (fromString k) viewer ]
    , not (null viewKeys) ]
  _ -> []


-- | A section directory's name without its leading ordering number.
dropOrdering :: String -> String
dropOrdering name = case span isDigit name of
  (_ : _, ' ' : rest) -> rest
  _ -> name
