{-# language TemplateHaskell, CPP #-}
module FractalStream.Metadata
  ( contributors
  , gitHash
  , gitTag
  , gitBranch
  , gitDirty
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
#if defined(wasm32_HOST_ARCH)
-- The wasm build's Template Haskell can't run git, as githash does, so
-- dev-tools/wasm/build.sh writes what git says into this module first.
import FractalStream.GitInfo (gitHash, gitTag, gitBranch, gitDirty)
#else
import GitHash
#endif

import qualified Data.ByteString.UTF8 as UTF8

contributorsFileContents :: ByteString
contributorsFileContents = $(embedFile "../CONTRIBUTORS")

contributors :: [String]
contributors = lines (UTF8.toString contributorsFileContents)

#if !defined(wasm32_HOST_ARCH)
gitInfo :: GitInfo
gitInfo = $$tGitInfoCwd

gitHash :: String
gitHash = giHash gitInfo

gitTag :: String
gitTag = giTag gitInfo

gitBranch :: String
gitBranch = giBranch gitInfo

gitDirty :: Bool
gitDirty = giDirty gitInfo
#endif
