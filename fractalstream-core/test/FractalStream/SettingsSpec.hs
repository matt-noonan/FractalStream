{-# language OverloadedStrings #-}
module FractalStream.SettingsSpec (spec) where

import Test.Hspec

import FractalStream.Settings
import FractalStream.Backends

import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map

spec :: Spec
spec = describe "FractalStream's settings file" $ do

  it "reads back what it writes, leaving out settings that are default" $ do
    let settings = defaultSettings { setProjectsFolder = Just "/somewhere/projects", setEmacsKeys = True
                                   , setFoldCodeBlocks = True, setMathSymbols = False
                                   , setEditorFont = Just "Iosevka", setEditorFontSize = Just 14
                                   , setLineNumbers = True, setSphereShading = 55
                                   , setRendering = [Cpu, GpuF64, GpuDf64, GpuF32]
                                   , setEditorColorsDark = Map.fromList
                                       [ ("keyword", noClassStyle { csColor = Just "#c792ea" })
                                       , ("undefined", noClassStyle { csColor = Just "#ff8080", csItalic = Just True
                                                                    , csBackground = Just "#202020" }) ] }
    readSettings "settings.yaml" (writeSettings settings) `shouldBe` (settings, [])
    BS8.unpack (writeSettings defaultSettings) `shouldBe` "{}\n"

  it "keeps settings it doesn't know about" $ do
    let (settings, warnings) = readSettings "settings.yaml" "projects-folder: /p\nfrom-the-future: 3\n"
    warnings `shouldBe` []
    setProjectsFolder settings `shouldBe` Just "/p"
    KM.toList (setOther settings) `shouldBe` [("from-the-future", Number 3)]
    fst (readSettings "settings.yaml" (writeSettings settings)) `shouldBe` settings

  it "ignores (and warns about) settings that don't make sense" $ do
    let (settings, warnings) = readSettings "settings.yaml" "projects-folder: 7\nemacs-keys: sometimes\nfold-code-blocks: 1\nmath-symbols: maybe\neditor-font: 12\neditor-font-size: big\nline-numbers: 3\nsphere-shading: 1.5\nrendering: fast\neditor-colors-light: {keyword: blue, number: {color: \"#123456\", sparkly: true}, string: \"#00aa00\"}\n"
    setProjectsFolder settings `shouldBe` Nothing
    setEmacsKeys settings `shouldBe` False
    setFoldCodeBlocks settings `shouldBe` False
    setMathSymbols settings `shouldBe` True
    setEditorFont settings `shouldBe` Nothing
    setEditorFontSize settings `shouldBe` Nothing
    setLineNumbers settings `shouldBe` False
    setSphereShading settings `shouldBe` defaultSphereShading
    setRendering settings `shouldBe` defaultBackendOrder
    setEditorColorsLight settings `shouldBe` Map.fromList [("string", noClassStyle { csColor = Just "#00aa00" })]
    length warnings `shouldBe` 11

  it "reads the backends' order, leaving out backends it doesn't know, and adding any left out" $ do
    readSettings "settings.yaml" "rendering: [gpu-f128, gpu-f32, cpu]\n"
      `shouldBe` (defaultSettings { setRendering = [GpuF32, Cpu, GpuF64, GpuDf64] }, [])

  it "puts GPU f64, new since orders were first saved, just before GPU df64" $ do
    readSettings "settings.yaml" "rendering: [gpu-df64, cpu, gpu-f32]\n"
      `shouldBe` (defaultSettings { setRendering = [GpuF64, GpuDf64, Cpu, GpuF32] }, [])
    readSettings "settings.yaml" "rendering: [cpu, gpu-f32, gpu-df64]\n"
      `shouldBe` (defaultSettings { setRendering = [Cpu, GpuF32, GpuF64, GpuDf64] }, [])

  it "uses the defaults for an empty file, or one that isn't YAML" $ do
    readSettings "settings.yaml" "" `shouldBe` (defaultSettings, [])
    fst (readSettings "settings.yaml" "projects-folder: [unclosed\n") `shouldBe` defaultSettings
