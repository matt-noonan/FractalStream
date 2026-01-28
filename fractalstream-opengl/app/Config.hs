{-# LANGUAGE DeriveGeneric #-}
module Config ( Config(..)
              , Viewer(..)
              , loadConfig ) where

import Data.Yaml
import GHC.Generics

import Graphics.Rendering.OpenGL (GLint, GLfloat)

loadConfig :: FilePath -> IO Config
loadConfig = decodeFileThrow

newtype Config = Config {viewers :: [Viewer]} deriving Generic

instance FromJSON Config

data Viewer = Viewer
  { title              :: String
  , width_pixels       :: Int
  , height_pixels      :: Int
  , coord              :: String
  , center_x           :: GLfloat
  , center_y           :: GLfloat
  , width              :: GLfloat
  , max_iterations     :: GLint
  , escape_radius      :: GLfloat
  , convergence_radius :: GLfloat
  , code               :: String
  } deriving Generic

instance FromJSON Viewer