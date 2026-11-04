{-# language OverloadedStrings #-}
module FractalStream.SettingsSpec (spec) where

import Test.Hspec

import FractalStream.Settings

import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8

spec :: Spec
spec = describe "FractalStream's settings file" $ do

  it "reads back what it writes, leaving out settings that are default" $ do
    let settings = defaultSettings { setProjectsFolder = Just "/somewhere/projects", setEmacsKeys = True
                                   , setFoldCodeBlocks = True, setMathSymbols = False
                                   , setEditorFont = Just "Iosevka", setEditorFontSize = Just 14
                                   , setLineNumbers = True }
    readSettings "settings.yaml" (writeSettings settings) `shouldBe` (settings, [])
    BS8.unpack (writeSettings defaultSettings) `shouldBe` "{}\n"

  it "keeps settings it doesn't know about" $ do
    let (settings, warnings) = readSettings "settings.yaml" "projects-folder: /p\nfrom-the-future: 3\n"
    warnings `shouldBe` []
    setProjectsFolder settings `shouldBe` Just "/p"
    KM.toList (setOther settings) `shouldBe` [("from-the-future", Number 3)]
    fst (readSettings "settings.yaml" (writeSettings settings)) `shouldBe` settings

  it "ignores (and warns about) settings that don't make sense" $ do
    let (settings, warnings) = readSettings "settings.yaml" "projects-folder: 7\nemacs-keys: sometimes\nfold-code-blocks: 1\nmath-symbols: maybe\neditor-font: 12\neditor-font-size: big\nline-numbers: 3\n"
    setProjectsFolder settings `shouldBe` Nothing
    setEmacsKeys settings `shouldBe` False
    setFoldCodeBlocks settings `shouldBe` False
    setMathSymbols settings `shouldBe` True
    setEditorFont settings `shouldBe` Nothing
    setEditorFontSize settings `shouldBe` Nothing
    setLineNumbers settings `shouldBe` False
    length warnings `shouldBe` 7

  it "uses the defaults for an empty file, or one that isn't YAML" $ do
    readSettings "settings.yaml" "" `shouldBe` (defaultSettings, [])
    fst (readSettings "settings.yaml" "projects-folder: [unclosed\n") `shouldBe` defaultSettings
