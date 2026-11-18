{-# language OverloadedStrings #-}

-- | Exporting a table from a project, and importing one into it (the
-- Export… and Import… buttons on a table's line in the configuration).
--
-- A table exports exactly as a zip of the same two files a project's
-- archive keeps it as ("Actor.SavedTables": its description,
-- @<name>.yaml@, and its elements, @<name>.bin@), which imports into any
-- table of the same type, replacing it with the file's: its size, origin
-- and step too.
--
-- A table of one or two dimensions of real numbers, integers, truth
-- values or complex numbers also exports as CSV, for other programs: a
-- 1-dimensional table a value a line; a 2-dimensional one of size
-- @[w, h]@ as @h@ lines of @w@ values (the first index along the line); a
-- complex number as two values, its real part then its imaginary part;
-- truth values as 0 and 1. A CSV imports into such a table, its size from
-- the CSV's shape, keeping the table's origin and step (which a CSV
-- doesn't say).
module Actor.TableTransfer
  ( exportTableZip
  , importTableZip
  , csvAvailable
  , exportTableCsv
  , importTableCsv
  , TableFormat(..)
  , exportTableFile
  , importTableFile
  , tableSummary
  ) where

import FractalStream.Prelude

import Actor.SavedTables
import Data.DynamicValue
import Data.Zip (isZip, readZip, writeZip, Packing(..))
import Language.Table
import Language.Type

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as Map
import Data.Char (isSpace, toLower)
import Numeric (readFloat, readSigned)
import qualified GHC.TypeNats as TN

-- | The two forms a table can be exported in.
data TableFormat = ZipFormat | CsvFormat
  deriving (Eq, Show)

-- | A table's value, exported in this form (the zip's files named for the
-- table, and packed as given: the web app's page deflates them itself).
exportTableFile :: Packing -> TableFormat -> String -> SomeType -> Variable Table -> IO (Either String BL.ByteString)
exportTableFile packing format name (SomeType ty) var = do
  t <- getDynamic var
  pure $ case format of
    ZipFormat -> exportTableZip packing name ty t
    CsvFormat -> exportTableCsv ty t

-- | Replace a table's value with a file's (an exported table, or a CSV,
-- told apart by their first bytes), or say why it can't be, leaving the
-- table as it was.
importTableFile :: SomeType -> Variable Table -> BS.ByteString -> IO (Either String ())
importTableFile (SomeType ty) var bytes = do
  imported <- if isZip bytes then importTableZip ty bytes
              else if csvAvailable ty then getDynamic var >>= \current -> importTableCsv ty current bytes
              else pure (Left "This isn't an exported table (a .zip), and this table can't be read from CSV.")
  case imported of
    Left why -> pure (Left why)
    Right t -> Right <$> setValue' var t

-- | What a table is, and its size: "Table 2 of ℝ, 100 × 50" (or "empty").
tableSummary :: SomeType -> Table -> String
tableSummary (SomeType ty) t =
  ppType ty ++ ", " ++ (if tableCellCount t == 0 then "empty" else intercalate " × " (map show (tableDims t)))

-- | A table, of this type, exported (as a zip of @<name>.yaml@ and
-- @<name>.bin@).
exportTableZip :: Packing -> String -> TypeProxy t -> Table -> Either String BL.ByteString
exportTableZip packing name ty t = case saveTable ty t of
  Nothing -> Left "this table can't be exported"
  Just st -> writeZip packing (tableFiles name st)

-- | A table of this type, from an exported one (a zip of its description
-- and its elements), or why it can't be.
importTableZip :: TypeProxy t -> BS.ByteString -> IO (Either String Table)
importTableZip ty bytes = case readZip bytes of
  Left _ -> pure (Left "This isn't a FractalStream table: it isn't a zip file.")
  Right entries -> case [ path | (path, _) <- entries, ".yaml" `isSuffixOf` path ] of
    [] -> pure (Left "This isn't a FractalStream table: there's no table description in it.")
    [path] -> case readTableAt (Map.fromList entries) path of
      Left why -> pure (Left ("This table can't be read: " ++ why ++ "."))
      Right st -> case ty of
        TableType n elemTy
          | stType st /= ppType ty ->
              pure (Left ("This is a " ++ stType st ++ ", but the table is a " ++ ppType ty ++ "."))
          | length (stSize st) /= dimsOf n ->
              pure (Left ("This table has " ++ show (length (stSize st)) ++ " dimensions, but the table has "
                          ++ show (dimsOf n) ++ "."))
          | otherwise -> either (\why -> Left ("This table can't be read: " ++ why ++ ".")) Right
                           <$> loadSavedTable elemTy st
        _ -> pure (Left "This isn't a table.")
    _ -> pure (Left "This holds more than one table description; a table exports as one.")

-- | Whether a table of this type can be exported and imported as CSV:
-- one or two dimensions, of real numbers, integers, truth values or
-- complex numbers.
csvAvailable :: TypeProxy t -> Bool
csvAvailable = \case
  TableType n elemTy -> dimsOf n `elem` [1, 2] && isJust (csvElement elemTy)
  _ -> False

-- | How an element is written in CSV, and read: its values (one, or two
-- for a complex number).
data CsvElement t = CsvElement
  { ceWidth :: Int
  , ceShow  :: HaskellType t -> [String]
  , ceRead  :: [String] -> Maybe (HaskellType t) }

csvElement :: TypeProxy t -> Maybe (CsvElement t)
csvElement = \case
  RealType -> Just (CsvElement 1 (\x -> [showReal x]) (\case { [s] -> readReal s; _ -> Nothing }))
  IntegerType -> Just (CsvElement 1 (\i -> [show i]) (\case { [s] -> readInteger s; _ -> Nothing }))
  BooleanType -> Just (CsvElement 1 (\b -> [if b then "1" else "0"]) (\case { [s] -> readBool s; _ -> Nothing }))
  ComplexType -> Just (CsvElement 2 (\z -> [showReal (realPart z), showReal (imagPart z)])
                                    (\case { [a, b] -> (:+) <$> readReal a <*> readReal b; _ -> Nothing }))
  _ -> Nothing

-- | A table of this type as CSV (see the module's description).
exportTableCsv :: TypeProxy t -> Table -> Either String BL.ByteString
exportTableCsv ty t = case ty of
  TableType _ elemTy | Just ce <- csvElement elemTy -> do
    let cell ixs = maybe (Left "a cell can't be read") (Right . ceShow ce) (readCell elemTy t ixs)
        line :: [[String]] -> BL.ByteString
        line cells = BL8.pack (intercalate "," (concat cells) ++ "\n")
    case tableDims t of
      [w] -> BL.concat <$> mapM (\j -> (\c -> line [c]) <$> cell [j]) [1 .. w]
      [w, h] -> BL.concat <$> mapM (\k -> line <$> mapM (\j -> cell [j, k]) [1 .. w]) [1 .. h]
      _ -> Left "only a table of one or two dimensions can be exported as CSV"
  _ -> Left "this table can't be exported as CSV"

-- | A table of this type from CSV, keeping this one's origin and step.
importTableCsv :: TypeProxy t -> Table -> BS.ByteString -> IO (Either String Table)
importTableCsv ty current bytes = case ty of
  TableType n elemTy | Just ce <- csvElement elemTy -> do
    let rows = filter (not . all null) (map (map trim . splitOn ',') (lines (map crToSpace (decodeUtf8 bytes))))
        width = ceWidth ce
        dims = dimsOf n
        cellsOf lineNo vs
          | length vs `mod` width /= 0 =
              Left ("line " ++ show lineNo ++ " has " ++ show (length vs) ++ " values, which isn't a whole number of "
                    ++ (if width == 2 then "complex numbers (two values each)" else "cells"))
          | otherwise = mapM (\(k, c) -> maybe (Left ("line " ++ show lineNo ++ ", value " ++ show k ++ ": "
                                                       ++ show (unwords c) ++ " isn't " ++ what elemTy))
                                                Right (ceRead ce c))
                             (zip [(1 :: Int) ..] (chunks width vs))
        parsed = zipWithM cellsOf [(1 :: Int) ..] rows
    case parsed of
      Left why -> pure (Left ("This CSV can't be read: " ++ why ++ "."))
      Right [] -> pure (Left "This CSV has no values in it.")
      Right cellRows -> do
        let shaped = case (dims, cellRows) of
              -- (a value a line, or all on one line)
              (1, _) | all ((== 1) . length) cellRows -> Right ([length cellRows], concat cellRows)
              (1, [one]) -> Right ([length one], one)
              (1, _) -> Left "a table of one dimension takes a value a line (or all of them on one line)"
              (_, r : _) | all ((== length r) . length) cellRows ->
                             Right ([length r, length cellRows], concat cellRows)
              _ -> Left "its lines don't all have the same number of values"
        case shaped of
          Left why -> pure (Left ("This CSV can't be read: " ++ why ++ "."))
          Right (size, values) -> newTable elemTy size (tableOrigin current) (tableStep current) >>= \case
            Nothing -> pure (Left "A table of that size can't be made.")
            Just t -> do
              forM_ (zip (indices size) values) $ \(ixs, v) -> writeCell elemTy t ixs v
              freezeTable t
              pure (Right t)
  _ -> pure (Left "This table can't be imported from CSV.")
  where
    crToSpace c = if c == '\r' then ' ' else c
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    chunks k xs = if null xs then [] else take k xs : chunks k (drop k xs)
    -- (in the order 'exportTableCsv' writes them: the first index fastest)
    indices :: [Int] -> [[Int]]
    indices = \case
      [w] -> [ [j] | j <- [1 .. w] ]
      [w, h] -> [ [j, k] | k <- [1 .. h], j <- [1 .. w] ]
      _ -> []
    what :: TypeProxy t' -> String
    what = \case
      RealType -> "a real number"
      IntegerType -> "an integer"
      BooleanType -> "0 or 1"
      ComplexType -> "a real number"
      _ -> "a value"

-- | A table type's number of dimensions.
dimsOf :: TN.SNat n -> Int
dimsOf = fromIntegral . TN.fromSNat

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, _ : rest) -> a : splitOn c rest
  (a, []) -> [a]

decodeUtf8 :: BS.ByteString -> String
decodeUtf8 = map (toEnum . fromIntegral) . BS.unpack   -- (the CSV's values are ASCII)

-- | A real number as CSV: as Haskell shows it (which other programs read),
-- but for infinities and NaN, as numpy and spreadsheets write them.
showReal :: Double -> String
showReal x
  | isNaN x = "nan"
  | isInfinite x = if x > 0 then "inf" else "-inf"
  | otherwise = show x

readReal :: String -> Maybe Double
readReal s0 = case map toLower s0 of
  s | s `elem` ["nan", "+nan", "-nan"] -> Just (0 / 0)
    | s `elem` ["inf", "+inf", "infinity", "+infinity"] -> Just (1 / 0)
    | s `elem` ["-inf", "-infinity"] -> Just (-1 / 0)
  _ -> case readSigned readFloat (fixup s0) of
    [(x, "")] -> Just x
    _ -> Nothing
  where
    -- (".5" and "-.5" and "+1": as Haskell doesn't read them)
    fixup = \case
      '+' : rest -> fixup rest
      '-' : '.' : rest -> "-0." ++ rest
      '.' : rest -> "0." ++ rest
      s -> s

readInteger :: String -> Maybe Int64
readInteger s = case readSigned readFloat (dropWhile (== '+') s) of
  [(x, "")] | x == fromIntegral (round x :: Integer), abs x <= 2147483647 -> Just (round (x :: Double))
  _ -> Nothing

readBool :: String -> Maybe Bool
readBool s = case map toLower s of
  "0" -> Just False
  "1" -> Just True
  "false" -> Just False
  "true" -> Just True
  _ -> Nothing
