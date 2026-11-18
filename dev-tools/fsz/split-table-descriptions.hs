-- Convert .fsz project archives to the current table layout
-- (PROJECT_FILES.md, 2026-11-17): from a manifest that described each
-- table itself,
--
--   manifest:
--     tables:
--       crits:
--         file: tables/crits.bin
--         type: ...
--
-- to one that names each table's description, beside its data:
--
--   manifest:
--     tables:
--       crits: tables/crits.yaml
--
-- Each table's data is checked against its old checksum first, and the
-- project's script and other keys are left as they were. In an archive
-- already in the current layout, the descriptions are written again, as
-- FractalStream writes them now (so running it twice changes nothing).
--
--   cd fractalstream-core && stack runghc ../dev-tools/fsz/split-table-descriptions.hs A.fsz [B.fsz ...]
{-# LANGUAGE LambdaCase, OverloadedStrings, PackageImports #-}
module Main (main) where

import Actor.Ensemble (rewriteProjectFile)
import Actor.SavedTables (SavedTable(..), tableFiles, readTableAt)
import "fractalstream-core" Data.Zip (readZip, writeZip, Packing(..))

import Control.Monad (forM, forM_, unless)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Yaml as YAML
import Numeric (showHex)
import System.Directory (renameFile)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = getArgs >>= mapM_ convert

convert :: FilePath -> IO ()
convert path = do
  bytes <- BS.readFile path
  entries <- either (die . ((path ++ ": ") ++)) pure (readZip bytes)
  let files = Map.fromList entries
  yaml <- maybe (die (path ++ ": no project.fs")) (pure . BL.toStrict) (Map.lookup "project.fs" files)
  o <- either (die . ((path ++ ": ") ++) . show) pure (YAML.decodeEither' yaml :: Either YAML.ParseException Object)
  let old = case KM.lookup "manifest" o of
        Just (Object m) | Just (Object t) <- KM.lookup "tables" m -> KM.toList t
        _ -> []
  if null old then putStrLn (path ++ ": no tables")
  else if all (\case { (_, String _) -> True; _ -> False }) old then do
    -- (the current layout: each description written again)
    tables <- forM old $ \(key, v) -> case v of
      String p -> either (\why -> die (path ++ ": table " ++ Key.toString key ++ ": " ++ why)) pure
                    (readTableAt files (Text.unpack p))
                  >>= \st -> pure (Key.toString key, Text.unpack p, st)
      _ -> die (path ++ ": table " ++ Key.toString key ++ " isn't in the current layout")
    let rewritten = Map.fromList (concat [ tableFiles (dropSuffix ".yaml" p) st | (_, p, st) <- tables ])
        new = [ (f, Map.findWithDefault b f rewritten) | (f, b) <- entries ]
    out <- either (die . ((path ++ ": ") ++)) pure (writeZip Deflated new)
    BL.writeFile (path ++ ".new") out
    renameFile (path ++ ".new") path
    putStrLn (path ++ ": " ++ show (length tables) ++ " descriptions written again")
  else do
      tables <- forM old $ \(key, v) -> do
        let name = Key.toString key
            fail' why = die (path ++ ": table " ++ name ++ ": " ++ why)
        entry <- either fail' pure (parseEither parseJSON v)
        (file, ty, size, origin, step, sha) <- either fail' pure (parseEither parseEntry entry)
        dat <- maybe (fail' (file ++ " isn't in the archive")) pure (Map.lookup file files)
        forM_ sha $ \expected -> unless (hex (SHA256.hashlazy dat) == expected) $
          fail' "its data doesn't match its checksum"
        pure (name, file, SavedTable ty size origin step dat Nothing)
      let manifest = object [ "tables" .= Map.fromList [ (n, "tables/" ++ n ++ ".yaml") | (n, _, _) <- tables ] ]
      project <- either (die . ((path ++ ": ") ++)) pure
                   (rewriteProjectFile id (KM.insert "manifest" manifest) yaml)
      let oldFiles = [ f | (_, f, _) <- tables ]
          kept = [ e | e@(f, _) <- entries, f /= "project.fs", f `notElem` oldFiles ]
          new = ("project.fs", BL.fromStrict project)
                : concat [ tableFiles ("tables/" ++ n) st | (n, _, st) <- tables ] ++ kept
      out <- either (die . ((path ++ ": ") ++)) pure (writeZip Deflated new)
      BL.writeFile (path ++ ".new") out
      renameFile (path ++ ".new") path
      putStrLn (path ++ ": " ++ show (length tables) ++ " tables converted")

dropSuffix :: String -> String -> String
dropSuffix suffix s
  | reverse suffix == take (length suffix) (reverse s) = take (length s - length suffix) s
  | otherwise = s

parseEntry :: Object -> YAML.Parser (FilePath, String, [Int], (Double, Double), (Double, Double), Maybe String)
parseEntry o = (,,,,,) <$> o .: "file" <*> o .: "type" <*> o .: "size" <*> o .: "origin" <*> o .: "step"
                       <*> o .:? "sha256"

hex :: BS.ByteString -> String
hex = concatMap (\w -> (if w < 16 then ('0' :) else id) (showHex w "")) . BS.unpack
