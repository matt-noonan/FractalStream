{-# language OverloadedStrings #-}

-- | A project file's @metadata:@ key: what a library of projects shows
-- about one without opening it -- a title, a plain-text description,
-- and a small preview image -- plus the scenario commands that produce
-- that preview (run by @fsrun --update-preview@).
--
-- Unlike a frontend's own keys (e.g. @wx-properties:@), this means the
-- same thing to every frontend, so the core reads and writes it itself.
module Actor.Metadata
  ( ProjectMetadata(..)
  , noMetadata
  , readMetadata
  , writeMetadata
  , isPng
  ) where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data ProjectMetadata = ProjectMetadata
  { mdTitle           :: Maybe String
  , mdDescription     :: Maybe String
  , mdPreviewCommands :: Maybe String
    -- ^ Scenario-language commands that set up and capture the
    -- preview, ending in @capture viewer "..." as preview@.
  , mdPreview         :: Maybe ByteString
    -- ^ A PNG image (raw bytes; base64 only in the file).
  , mdOther           :: KM.KeyMap Value
    -- ^ Any other keys under @metadata:@, kept verbatim, so that a
    -- file written by a newer version of FractalStream doesn't lose
    -- them by being saved with this one.
  }
  deriving (Eq, Show)

noMetadata :: ProjectMetadata
noMetadata = ProjectMetadata Nothing Nothing Nothing Nothing KM.empty

-- | Read a @metadata:@ key's value, along with warnings about anything
-- in it that had to be ignored. A broken preview only costs the
-- preview, not the whole project, so it's dropped rather than refusing
-- to open the file.
readMetadata :: Value -> Either String (ProjectMetadata, [String])
readMetadata = \case
  Object o -> do
    mdTitle           <- textField "title" o
    mdDescription     <- textField "description" o
    mdPreviewCommands <- textField "preview-commands" o
    (mdPreview, warnings) <- case KM.lookup "preview" o of
      Nothing -> Right (Nothing, [])
      Just (String t) ->
        case B64.decode (Text.encodeUtf8 (Text.filter (not . isSpace) t)) of
          Left err -> Right (Nothing, ["ignoring the metadata's preview, which isn't valid base64: " ++ err])
          Right png
            | isPng png -> Right (Just png, [])
            | otherwise -> Right (Nothing, ["ignoring the metadata's preview, which isn't a PNG image"])
      Just _ -> Left "the metadata's `preview` should be a base64-encoded PNG image"
    let mdOther = foldr KM.delete o ["title", "description", "preview-commands", "preview"]
    pure (ProjectMetadata{..}, warnings)
  _ -> Left "`metadata` should be a set of keys (title, description, preview-commands, preview)"
  where
    textField k o = case KM.lookup k o of
      Nothing         -> Right Nothing
      Just (String t) -> Right (Just (Text.unpack t))
      Just _          -> Left ("the metadata's `" ++ Key.toString k ++ "` should be text")

-- | The value to write under @metadata:@, or 'Nothing' if there's
-- nothing to write. The preview's base64 is broken into lines, so that
-- it's written as a block of text rather than as one enormous line.
writeMetadata :: ProjectMetadata -> Maybe Value
writeMetadata ProjectMetadata{..}
  | KM.null fields = Nothing
  | otherwise = Just (Object fields)
  where
    fields = KM.union (KM.fromList (catMaybes
      [ ("title" .=) <$> mdTitle
      , ("description" .=) <$> mdDescription
      , ("preview-commands" .=) <$> mdPreviewCommands
      , ("preview" .=) . base64Lines <$> mdPreview
      ])) mdOther
    base64Lines = Text.unlines . Text.chunksOf 76 . Text.decodeUtf8 . B64.encode

-- | Does this look like a PNG image? (Checks its signature only.)
isPng :: ByteString -> Bool
isPng = BS.isPrefixOf "\x89PNG\r\n\x1a\n"
