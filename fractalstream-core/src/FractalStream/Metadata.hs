{-# language TemplateHaskell #-}
module FractalStream.Metadata
  ( contributors
  , gitHash
  , gitBranch
  , gitDirty
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import GitHash

import qualified Data.ByteString.UTF8 as UTF8

contributorsFileContents :: ByteString
contributorsFileContents = $(embedFile "../CONTRIBUTORS")

contributors :: [String]
contributors = lines (UTF8.toString contributorsFileContents)

gitInfo :: GitInfo
gitInfo = $$tGitInfoCwd

gitHash :: String
gitHash = giHash gitInfo

gitBranch :: String
gitBranch = giBranch gitInfo

gitDirty :: Bool
gitDirty = giDirty gitInfo
