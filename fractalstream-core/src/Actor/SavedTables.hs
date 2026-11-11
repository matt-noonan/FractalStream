{-# language OverloadedStrings #-}

-- | A project file's @tables:@ key: what the project's tables held when it
-- was saved, by name, for the tables that weren't empty. Each one has its
-- type (as the script declares it), its size, where it lies in the plane
-- (the center of its cell (1, 1), and a cell's width and height), and its
-- elements, packed ('Language.Table.packTable'), compressed with zlib, and
-- written in base64, 76 characters to a line:
--
-- > tables:
-- >   crits:
-- >     type: Table 3 of (ℂ × ℤ)
-- >     size: [120, 120, 2]
-- >     origin: [-2.0, 1.5]
-- >     step: [0.025, 0.025]
-- >     data: |
-- >       eJzt3U1u2zAQ...
--
-- Like @values:@ ("Actor.SavedValues"), these are where a project was
-- left, not part of what it is, so the wx app restores them and fsrun
-- ignores them; and saving them is up to the user, since they can be big.
module Actor.SavedTables
  ( SavedTable(..)
  , SavedTables
  , readSavedTables
  , writeSavedTables
  , captureTables
  , applySavedTables
  , TableSummary(..)
  , summarizeTables
  ) where

import FractalStream.Prelude

import Data.DynamicValue
import Language.Type
import Language.Table

import Codec.Compression.Zlib (compress, decompress)
import Control.Exception (evaluate, try, SomeException)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | One saved table, as the project file has it.
data SavedTable = SavedTable
  { stType   :: String
  , stSize   :: [Int]
  , stOrigin :: (Double, Double)
  , stStep   :: (Double, Double)
  , stData   :: ByteString
    -- ^ the packed elements, compressed
  }
  deriving (Eq, Show)

type SavedTables = Map String SavedTable

instance FromJSON SavedTable where
  parseJSON = withObject "table" $ \o -> do
    stType <- o .: "type"
    stSize <- o .: "size"
    stOrigin <- o .: "origin"
    stStep <- o .: "step"
    encoded <- o .: "data"
    stData <- either (fail . ("its data isn't base64: " ++)) pure
                (B64.decode (Text.encodeUtf8 (Text.filter (`notElem` [' ', '\n', '\r', '\t']) encoded)))
    pure SavedTable{..}

instance ToJSON SavedTable where
  toJSON SavedTable{..} = object
    [ "type" .= stType
    , "size" .= stSize
    , "origin" .= stOrigin
    , "step" .= stStep
    , "data" .= (Text.unlines . Text.chunksOf 76 . Text.decodeUtf8 . B64.encode) stData
    ]

-- | Read the @tables:@ key out of a project file's frontend keys. A
-- project without one has no saved tables.
readSavedTables :: KM.KeyMap Value -> Either String SavedTables
readSavedTables = maybe (Right Map.empty) (parseEither parseJSON) . KM.lookup "tables"

-- | Replace the @tables:@ key (or remove it, when there are no tables to
-- save), leaving the project's other keys alone.
writeSavedTables :: SavedTables -> KM.KeyMap Value -> KM.KeyMap Value
writeSavedTables tables
  | Map.null tables = KM.delete "tables"
  | otherwise = KM.insert "tables" (toJSON tables)

-- | What the given tables hold, to save: the ones that aren't empty.
captureTables :: [(String, SomeType, Variable Table)] -> IO SavedTables
captureTables tables = fmap (Map.fromList . catMaybes) . forM tables $ \(name, SomeType ty, var) -> do
  t <- getDynamic var
  pure $ case ty of
    TableType _ elemTy | tableCellCount t > 0, Just packed <- packTable elemTy t ->
      Just (name, SavedTable
        { stType = ppType ty
        , stSize = tableDims t
        , stOrigin = tableOrigin t
        , stStep = tableStep t
        , stData = BL.toStrict (compress (BL.fromStrict packed)) })
    _ -> Nothing

-- | Put saved tables back into the given tables, returning a warning for
-- each one that couldn't be (because the project no longer has a table by
-- that name and type, or its data doesn't fit).
applySavedTables :: [(String, SomeType, Variable Table)] -> SavedTables -> IO [String]
applySavedTables tables saved = fmap catMaybes . forM (Map.toList saved) $ \(name, st) ->
  case [ (ty, var) | (name', ty, var) <- tables, name' == name ] of
    [] -> pure (Just ("ignoring the saved table `" ++ name ++ "`, which the project no longer has"))
    ((SomeType ty, var) : _)
      | ppType ty /= stType st ->
          pure (Just ("ignoring the saved table `" ++ name ++ "`: it was saved as a "
                      ++ stType st ++ ", but it's now a " ++ ppType ty))
      | TableType _ elemTy <- ty -> do
          let bad why = Just ("ignoring the saved table `" ++ name ++ "`: " ++ why)
          try (evaluate (BL.toStrict (decompress (BL.fromStrict (stData st))))) >>= \case
            Left (_ :: SomeException) -> pure (bad "its data is damaged")
            Right packed -> unpackTable elemTy (stSize st) (stOrigin st) (stStep st) packed >>= \case
              Left err -> pure (bad err)
              Right t -> do
                freezeTable t
                setValue' var t
                pure Nothing
      | otherwise -> pure (Just ("ignoring the saved table `" ++ name ++ "`, which isn't a table"))

-- | A table's name, size, and how many bytes its data takes before
-- compression, for telling the user what saving the tables would save.
data TableSummary = TableSummary
  { tsName  :: String
  , tsSize  :: [Int]
  , tsBytes :: Int
  }
  deriving (Eq, Show)

-- | The tables that aren't empty, as 'TableSummary's.
summarizeTables :: [(String, SomeType, Variable Table)] -> IO [TableSummary]
summarizeTables tables = fmap catMaybes . forM tables $ \(name, SomeType ty, var) -> do
  t <- getDynamic var
  pure $ case ty of
    TableType _ elemTy | tableCellCount t > 0, Just n <- packedElementSize elemTy ->
      Just (TableSummary name (tableDims t) (n * tableCellCount t))
    _ -> Nothing
