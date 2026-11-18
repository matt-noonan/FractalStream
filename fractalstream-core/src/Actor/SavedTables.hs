{-# language OverloadedStrings #-}

-- | What a project's tables held when it was saved (the tables that
-- weren't empty), which a project file keeps in a @.fsz@ archive (see
-- PROJECT_FILES.md). A table on disk is two files: its elements, packed
-- ('Language.Table.packTable'), and a description of them, beside it:
--
-- > # tables/crits.yaml
-- > type: Table 3 of (ℂ × ℤ)
-- > size: [120, 120, 2]
-- > origin: [-2.0, 1.5]
-- > step: [0.025, 0.025]
-- > encoding: raw
-- > file: crits.bin
-- > sha256: 3f2a...
--
-- (@file:@ is beside the description), and the project's @manifest:@
-- says where each table's description is, by name:
--
-- > manifest:
-- >   tables:
-- >     crits: tables/crits.yaml
--
-- The same two files, in a zip of their own, are a table exported from
-- a project (and imported into one).
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
  , tableFiles
  , readTableAt
  , loadSavedTable
  , saveTable
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
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as Map
import Data.IORef (newIORef, modifyIORef', readIORef)
import Control.Exception (SomeException, try)
import Numeric (showHex)
import Data.Char (isAlphaNum)
import qualified Data.Yaml as YAML

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

-- | A table's description: what's in its file, and where that is
-- (beside the description).
data TableInfo = TableInfo
  { tiFile :: FilePath, tiType :: String, tiSize :: [Int]
  , tiOrigin :: (Double, Double), tiStep :: (Double, Double), tiSha256 :: Maybe String }

instance FromJSON TableInfo where
  parseJSON = withObject "table" $ \o -> do
    encoding <- o .:? "encoding" .!= ("raw" :: String)
    unless (encoding == "raw") $
      fail ("its encoding is " ++ show encoding ++ ", which this version of FractalStream can't read")
    TableInfo <$> o .: "file" <*> o .: "type" <*> o .: "size" <*> o .: "origin" <*> o .: "step"
              <*> o .:? "sha256"

-- | The tables a project's @manifest:@ says it has (each by its
-- description's path), from its archive's files (by path); and a warning
-- for each one that can't be had.
readSavedTables :: KM.KeyMap Value -> Map FilePath BL.ByteString -> (SavedTables, [String])
readSavedTables manifest files = case KM.lookup "tables" manifest of
  Nothing -> (Map.empty, [])
  Just v -> case parseEither parseJSON v of
    Left err -> (Map.empty, ["ignoring the manifest's tables, which can't be read: " ++ err])
    Right (entries :: Map String FilePath) ->
      let one (name, path) = either (\why -> Left ("ignoring the saved table `" ++ name ++ "`: " ++ why))
                                    (\st -> Right (name, st))
                                    (readTableAt files path)
          results = map one (Map.toList entries)
      in (Map.fromList (rights results), lefts results)

-- | The table whose description is at this path among these files (its
-- elements' file beside it), or why it can't be had.
readTableAt :: Map FilePath BL.ByteString -> FilePath -> Either String SavedTable
readTableAt files path = do
  described <- maybe (Left ("its description " ++ show path ++ " isn't there")) Right (Map.lookup path files)
  TableInfo{..} <- either (\e -> Left ("its description can't be read: " ++ show e)) Right
                     (YAML.decodeEither' (BL.toStrict described))
  let file = folderOf path ++ tiFile
  bytes <- maybe (Left ("its file " ++ show file ++ " isn't there")) Right (Map.lookup file files)
  pure (SavedTable tiType tiSize tiOrigin tiStep bytes tiSha256)
  where
    -- (archive paths: always with /, whatever the platform)
    folderOf p = reverse (dropWhile (/= '/') (reverse p))

-- | A table's two files, @<base>.yaml@ (its description) and @<base>.bin@
-- (its elements), @base@ a path with /. The description is written a
-- field a line, in a fixed order, lists inline (each value as JSON, which
-- YAML reads as it is).
tableFiles :: FilePath -> SavedTable -> [(FilePath, BL.ByteString)]
tableFiles base SavedTable{..} =
  [ (base ++ ".yaml", BL.concat [ field k v | (k, v) <- description ])
  , (base ++ ".bin", stData) ]
  where
    fileName = reverse (takeWhile (/= '/') (reverse base)) ++ ".bin"
    description =
      [ ("type", json stType), ("size", list (map json stSize))
      , ("origin", pair stOrigin), ("step", pair stStep)
      , ("encoding", "raw"), ("file", plainOrJson fileName)
      , ("sha256", BL8.pack (hex (SHA256.hashlazy stData))) ]
    field k v = BL.concat [k, ": ", v, "\n"]
    json :: ToJSON a => a -> BL.ByteString
    json = encode
    pair (a, b) = list [json a, json b]
    -- (a file name as it is, if YAML would read it so; else quoted)
    plainOrJson name
      | not (null name), all (\c -> isAlphaNum c || c `elem` ("_.-" :: String)) name = BL8.pack name
      | otherwise = json name
    list xs = BL.concat ["[", BL.intercalate ", " xs, "]"]

-- | The @manifest:@ entries for these tables, and their files (each
-- table's at @tables/<name>.yaml@ and @tables/<name>.bin@), to write into
-- a project's archive.
writeSavedTables :: SavedTables -> (Value, [(FilePath, BL.ByteString)])
writeSavedTables tables =
  ( object [ "tables" .= Map.fromList [ (name, "tables/" ++ name ++ ".yaml") | name <- Map.keys tables ] ]
  , concat [ tableFiles ("tables/" ++ name) st | (name, st) <- Map.toList tables ] )

hex :: BS.ByteString -> String
hex = concatMap (\w -> (if w < 16 then ('0' :) else id) (showHex w "")) . BS.unpack

-- | What the given tables hold, to save: the ones that aren't empty.
captureTables :: [(String, SomeType, Variable Table)] -> IO SavedTables
captureTables tables = fmap (Map.fromList . catMaybes) . forM tables $ \(name, SomeType ty, var) -> do
  t <- getDynamic var
  pure $ case saveTable ty t of
    Just st | tableCellCount t > 0 -> Just (name, st)
    _ -> Nothing

-- | A table of this type, to save (or Nothing, if the type isn't a
-- table's, or can't be packed).
saveTable :: TypeProxy t -> Table -> Maybe SavedTable
saveTable ty t = case ty of
  TableType _ elemTy -> do
    packed <- packTable elemTy t
    pure SavedTable
      { stType = ppType ty
      , stSize = tableDims t
      , stOrigin = tableOrigin t
      , stStep = tableStep t
      , stData = BL.fromStrict packed
      , stSha256 = Nothing }
  _ -> Nothing

-- | Put saved tables back into the given tables, returning a warning for
-- each one that couldn't be (because the project no longer has a table by
-- that name and type, or its data doesn't fit, or isn't what its
-- description says). Each table's data is read once, into the table, checked as it
-- goes by.
applySavedTables :: [(String, SomeType, Variable Table)] -> SavedTables -> IO [String]
applySavedTables tables saved = fmap catMaybes . forM (Map.toList saved) $ \(name, st) ->
  case [ (ty, var) | (name', ty, var) <- tables, name' == name ] of
    [] -> pure (Just ("ignoring the saved table `" ++ name ++ "`, which the project no longer has"))
    ((SomeType ty, var) : _)
      | ppType ty /= stType st ->
          pure (Just ("ignoring the saved table `" ++ name ++ "`: it was saved as a "
                      ++ stType st ++ ", but it's now a " ++ ppType ty))
      | TableType _ elemTy <- ty -> loadSavedTable elemTy st >>= \case
          Left why -> pure (Just ("ignoring the saved table `" ++ name ++ "`: " ++ why))
          Right t -> Nothing <$ setValue' var t
      | otherwise -> pure (Just ("ignoring the saved table `" ++ name ++ "`, which isn't a table"))

-- | A saved table's contents, as a table of these elements (frozen), or
-- why they can't be: their data doesn't fit their size, or isn't what
-- their description says (its checksum). Its data is read once, into the
-- table, checked as it goes by.
loadSavedTable :: TypeProxy t -> SavedTable -> IO (Either String Table)
loadSavedTable elemTy st = do
  hash <- newIORef SHA256.init
  try (unpackTableChunks elemTy (stSize st) (stOrigin st) (stStep st) (stData st)
         (\chunk -> modifyIORef' hash (`SHA256.update` chunk))) >>= \case
    Left (_ :: SomeException) -> pure (Left "its data is damaged")
    Right (Left err) -> pure (Left err)
    Right (Right t) -> do
      actual <- hex . SHA256.finalize <$> readIORef hash
      if maybe False (/= actual) (stSha256 st)
        then pure (Left "its data isn't what its description says (its checksum doesn't match)")
        else Right t <$ freezeTable t

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
