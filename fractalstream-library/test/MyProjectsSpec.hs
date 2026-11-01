-- | Reading a user's own projects from their projects folder
-- ('scanProjectsIn').
module MyProjectsSpec (spec) where

import Test.Hspec

import FractalStream.Library

import Data.Maybe (isJust)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))

spec :: Spec
spec = describe "scanning the user's projects folder" $ do

  it "lists the projects in it under \"My projects\", and each subfolder's \
     \as a section (in order, without their ordering numbers)" $
    withFolder $ \dir -> do
      sections <- scanProjectsIn dir
      [ (lsName s, map leTitle (lsEntries s)) | s <- sections ] `shouldBe`
        [ ("My projects", ["A titled project", "bad", "untitled"])
        , ("Basics", ["broken", "d"])
        , ("Spirals", ["c"])
        ]
      all lsMine sections `shouldBe` True

  it "opens projects in place, says which have problems, and which of \
     \those can still be opened (to fix their scripts)" $
    withFolder $ \dir -> do
      root : basics : _ <- scanProjectsIn dir
      [ (leTitle e, isJust (leProblem e), leCanOpen e) | e <- lsEntries basics ] `shouldBe`
        [ ("broken", True, True), ("d", False, True) ]
      [ (leTitle e, leFile e, isJust (leProblem e), leCanOpen e) | e <- lsEntries root ] `shouldBe`
        [ ("A titled project", Just (dir </> "titled.fs"), False, True)
        , ("bad", Just (dir </> "bad.fs"), True, False)
        , ("untitled", Just (dir </> "untitled.fs"), False, True)
        ]

  it "finds the documents folder a Linux desktop's user-dirs.dirs names" $ do
    let home = "/home/someone"
    xdgDocumentsDirectory home "XDG_DESKTOP_DIR=\"$HOME/Desktop\"\nXDG_DOCUMENTS_DIR=\"$HOME/Dokumente\"\n"
      `shouldBe` Just (home </> "Dokumente")
    xdgDocumentsDirectory home "XDG_DOCUMENTS_DIR=\"/data/docs\"\n" `shouldBe` Just "/data/docs"
    xdgDocumentsDirectory home "XDG_DOCUMENTS_DIR=\"$HOME\"\n" `shouldBe` Nothing
    xdgDocumentsDirectory home "# no documents folder here\n" `shouldBe` Nothing

  it "has an empty \"My projects\" section when the folder doesn't exist" $ do

    tmp <- getTemporaryDirectory
    sections <- scanProjectsIn (tmp </> "fractalstream-no-such-folder")
    [ (lsName s, length (lsEntries s)) | s <- sections ] `shouldBe` [("My projects", 0)]

-- | A projects folder with a bit of everything in it.
withFolder :: (FilePath -> IO a) -> IO a
withFolder action = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "fractalstream-my-projects-test"
      project = "script: |\n  @viewer \"V\"\n  pass\n"
  createDirectoryIfMissing True (dir </> "2 Spirals")
  createDirectoryIfMissing True (dir </> "1 Basics" </> "too deep")
  writeFile (dir </> "titled.fs") (project ++ "metadata:\n  title: A titled project\n")
  writeFile (dir </> "untitled.fs") project
  writeFile (dir </> "bad.fs") "script: [this isn't\n"
  writeFile (dir </> "1 Basics" </> "broken.fs") "script: |\n  @viewer \"V\"\n  letz x = 1\n"
  writeFile (dir </> ".hidden.fs") project
  writeFile (dir </> "notes.txt") "not a project"
  writeFile (dir </> "2 Spirals" </> "c.fs") project
  writeFile (dir </> "1 Basics" </> "d.fs") project
  writeFile (dir </> "1 Basics" </> "too deep" </> "e.fs") project
  result <- action dir
  removeDirectoryRecursive dir
  pure result
