{-# LANGUAGE DeriveGeneric #-}
module Config ( Config(..)
              , Viewer(..)
              , loadConfig ) where

import Data.Yaml
import GHC.Generics

loadConfig :: FilePath -> IO Config
loadConfig = decodeFileThrow

data Config = Config
  { viewers :: [Viewer] 
  } deriving Generic

instance FromJSON Config

data Viewer = Viewer
  { title         :: String
  , width_pixels  :: Int
  , height_pixels :: Int
  , coord         :: String
  , center_x      :: Float
  , center_y      :: Float
  , radius        :: Float
  , code          :: String
  } deriving Generic

instance FromJSON Viewer