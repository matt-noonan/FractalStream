{-# language OverloadedStrings #-}

-- | What a project's tables held when it was saved (the tables that
-- weren't empty), which a project file keeps in a @.fsz@ archive (see
-- PROJECT_FILES.md): each table's elements in a file of their own, packed
-- ('Language.Table.packTable'), and described in the project's
-- @manifest:@ key, by name:
--
-- > manifest:
-- >   tables:
-- >     crits:
-- >       file: tables/crits.bin
-- >       type: Table 3 of (ℂ × ℤ)
-- >       size: [120, 120, 2]
-- >       origin: [-2.0, 1.5]
-- >       step: [0.025, 0.025]
-- >       encoding: raw
-- >       sha256: 3f2a...
--
-- Like @values:@ ("Actor.SavedValues"), these are where a project was
-- left, not part of what it is, so the apps restore them and fsrun
-- ignores them (unless asked); and saving them is up to the user, since
-- they can be big.
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

import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.IORef (newIORef, modifyIORef', readIORef)
import Control.Exception (SomeException, try)
import Numeric (showHex)

-- | One saved table.
data SavedTable = SavedTable
  { stType   :: String
  , stSize   :: [Int]
  , stOrigin :: (Double, Double)
  , stStep   :: (Double, Double)
  , stData   :: BL.ByteString
    -- ^ the packed elements (read from the archive as they're needed)
  , stSha256 :: Maybe String
    -- ^ what their SHA-256 should be, as the manifest says (in hex)
  }

type SavedTables = Map String SavedTable

-- | A manifest's entry for a table: where its file is, and what's in it.
data TableEntry = TableEntry
  { teFile :: FilePath, teType :: String, teSize :: [Int]
  , teOrigin :: (Double, Double), teStep :: (Double, Double), teSha256 :: Maybe String }

instance FromJSON TableEntry where
  parseJSON = withObject "table" $ \o -> do
    encoding <- o .:? "encoding" .!= ("raw" :: String)
    unless (encoding == "raw") $
      fail ("its encoding is " ++ show encoding ++ ", which this version of FractalStream can't read")
    TableEntry <$> o .: "file" <*> o .: "type" <*> o .: "size" <*> o .: "origin" <*> o .: "step"
               <*> o .:? "sha256"

-- | The tables a project's @manifest:@ says it has, from its archive's
-- files (by path); and a warning for each one that can't be had.
readSavedTables :: KM.KeyMap Value -> Map FilePath BL.ByteString -> (SavedTables, [String])
readSavedTables manifest files = case KM.lookup "tables" manifest of
  Nothing -> (Map.empty, [])
  Just v -> case parseEither parseJSON v of
    Left err -> (Map.empty, ["ignoring the manifest's tables, which can't be read: " ++ err])
    Right (entries :: Map String Value) ->
      let one (name, e) = case parseEither parseJSON e of
            Left err -> Left ("ignoring the saved table `" ++ name ++ "`: " ++ err)
            Right TableEntry{..} -> case Map.lookup teFile files of
              Nothing -> Left ("ignoring the saved table `" ++ name ++ "`: its file " ++ show teFile
                               ++ " isn't in the project's archive")
              Just bytes -> Right (name, SavedTable teType teSize teOrigin teStep bytes teSha256)
          results = map one (Map.toList entries)
      in (Map.fromList (rights results), lefts results)

-- | The @manifest:@ entries for these tables, and their files (each
-- table's at @tables/<name>.bin@), to write into a project's archive.
writeSavedTables :: SavedTables -> (Value, [(FilePath, BL.ByteString)])
writeSavedTables tables =
  ( object [ "tables" .= Map.fromList [ (name, entry name st) | (name, st) <- Map.toList tables ] ]
  , [ (file name, stData st) | (name, st) <- Map.toList tables ] )
  where
    file name = "tables/" ++ name ++ ".bin"
    entry name SavedTable{..} = object
      [ "file" .= file name, "type" .= stType, "size" .= stSize
      , "origin" .= stOrigin, "step" .= stStep, "encoding" .= ("raw" :: String)
      , "sha256" .= hex (SHA256.hashlazy stData) ]

hex :: BS.ByteString -> String
hex = concatMap (\w -> (if w < 16 then ('0' :) else id) (showHex w "")) . BS.unpack

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
        , stData = BL.fromStrict packed
        , stSha256 = Nothing })
    _ -> Nothing

-- | Put saved tables back into the given tables, returning a warning for
-- each one that couldn't be (because the project no longer has a table by
-- that name and type, or its data doesn't fit, or isn't what the manifest
-- says). Each table's data is read once, into the table, checked as it
-- goes by.
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
          hash <- newIORef SHA256.init
          try (unpackTableChunks elemTy (stSize st) (stOrigin st) (stStep st) (stData st)
                 (\chunk -> modifyIORef' hash (`SHA256.update` chunk))) >>= \case
            Left (_ :: SomeException) -> pure (bad "its data is damaged")
            Right (Left err) -> pure (bad err)
            Right (Right t) -> do
              actual <- hex . SHA256.finalize <$> readIORef hash
              if maybe False (/= actual) (stSha256 st)
                then pure (bad "its data isn't what the manifest says (its checksum doesn't match)")
                else do
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
