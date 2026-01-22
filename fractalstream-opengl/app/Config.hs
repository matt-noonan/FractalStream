{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Config ( Config(..)
              , Viewer(..)
              , loadConfig 
              ) where

import Data.Yaml
import Text.Read

import qualified Data.Text as T

loadConfig :: FilePath -> IO Config
loadConfig = decodeFileThrow

data Config = Config
  { viewer :: Viewer }

instance FromJSON Config where
  parseJSON = withObject "project" $ \o -> do
    viewer <- o .: "viewer"
    pure Config{..}

data Viewer = Viewer
  { title    :: String
  , pxWidth  :: Int
  , pxHeight :: Int
  , coord    :: String
  , center   :: Float
  , radius   :: Float
  , source   :: String
  }

instance FromJSON Viewer where
  parseJSON = withObject "viewer" $ \o -> do
    title                      <- o .: "title"
    PixelSize pxWidth pxHeight <- o .: "size"
    coord                      <- o .: "z-coord"
    center                     <- o .: "initial-center"
    radius                     <- o .: "initial-radius"
    source                     <- o .: "code"
    pure Viewer{..}

data PixelSize = PixelSize Int Int

instance FromJSON PixelSize where
  parseJSON = withText "dimensions" $ \txt -> do
    case T.splitOn "x" txt of
      [xStr, yStr] -> do
        case (,) <$> readMaybe (T.unpack xStr) <*> readMaybe (T.unpack yStr) of
          Just (w,h) -> pure $ PixelSize w h
          Nothing  -> fail "could not parse dimension descriptor"
      _ -> fail "expected a dimension descriptor, e.g. 400x200"