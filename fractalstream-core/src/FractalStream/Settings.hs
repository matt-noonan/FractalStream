{-# language OverloadedStrings #-}

-- | FractalStream's own settings (as opposed to a project's): a plain
-- YAML file in the conventional place for each system ('settingsFile'),
-- written by FractalStream itself rather than through any GUI toolkit's
-- preferences mechanism, so that it's easy to find, read, back up, or
-- delete, and so that any frontend can read it.
--
-- Every setting has a default, so a missing file is just the defaults.
-- A file that can't be read, or a setting that doesn't make sense, is
-- reported and replaced by the default rather than stopping anything;
-- and keys this version doesn't know are kept as they are, so a newer
-- version's settings survive being saved by an older one.
module FractalStream.Settings
  ( Settings(..)
  , defaultSettings
  , defaultEditorFont
  , editorFont
  , settingsFile
  , loadSettings
  , saveSettings
  , readSettings
  , writeSettings
  ) where

import FractalStream.Quoted (quoted)
import Control.Exception (IOException, try)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Yaml as YAML
import System.Directory (XdgDirectory(..), createDirectoryIfMissing, doesFileExist,
                         getHomeDirectory, getXdgDirectory, renameFile)
import System.FilePath ((</>), takeDirectory)
import System.Info (os)

data Settings = Settings
  { setProjectsFolder :: Maybe FilePath
    -- ^ Where the user's own projects are, or 'Nothing' for the default
    -- (a @FractalStream@ folder in their documents folder).
  , setEditorFont     :: Maybe String
    -- ^ The font code editors use, by name, or 'Nothing' for the
    -- platform's usual fixed-width font ('defaultEditorFont').
  , setEditorFontSize :: Maybe Int
    -- ^ The size of that font, in points, or 'Nothing' for the editor's
    -- usual size.
  , setLineNumbers    :: Bool
    -- ^ Whether code editors show line numbers.
  , setEmacsKeys      :: Bool
    -- ^ Whether code editors have emacs-style movement keys (Control-A,
    -- Control-E, ...) where Control is the standard shortcut key, as on
    -- Windows and Linux. (On macOS they always do.)
  , setMathSymbols    :: Bool
    -- ^ Whether code editors rewrite scripts with mathematical symbols
    -- (⟵, ℝ, z², ...) in place of ASCII spellings, as they're edited.
  , setFoldCodeBlocks :: Bool
    -- ^ Whether code editors let blocks of code (an @if@'s body, a
    -- loop's, ...) be folded away, as well as stanzas and tool handlers.
  , setOther          :: KM.KeyMap Value
    -- ^ Keys this version doesn't know, kept as they are.
  }
  deriving (Eq, Show)

defaultSettings :: Settings
defaultSettings = Settings Nothing Nothing Nothing False False True False KM.empty

-- | The platform's usual fixed-width font.
defaultEditorFont :: String
defaultEditorFont = case os of
  "darwin"  -> "Monaco"
  "mingw32" -> "Consolas"
  _         -> "Monospace"

-- | The font code editors use.
editorFont :: Settings -> String
editorFont = maybe defaultEditorFont id . setEditorFont

-- | Where the settings are kept: in the user's application support
-- folder on macOS, their roaming application data on Windows, and
-- their configuration folder (as XDG has it) elsewhere.
settingsFile :: IO FilePath
settingsFile = (</> "settings.yaml") <$> case os of
  "darwin"  -> (</> "Library" </> "Application Support" </> "FractalStream") <$> getHomeDirectory
  "mingw32" -> getXdgDirectory XdgConfig "FractalStream"  -- %APPDATA% on Windows
  _         -> getXdgDirectory XdgConfig "fractalstream"

-- | Read the settings, along with warnings about anything in them that
-- had to be ignored. Never fails: whatever can't be read is the default.
loadSettings :: IO (Settings, [String])
loadSettings = do
  path <- settingsFile
  doesFileExist path >>= \case
    False -> pure (defaultSettings, [])
    True -> try (BS.readFile path) >>= \case
      Left (e :: IOException) -> pure (defaultSettings, ["couldn't read " ++ quoted path ++ ": " ++ show e])
      Right contents -> pure (readSettings path contents)

-- | Read settings from a settings file's contents (@path@ says where
-- they came from, in warnings).
readSettings :: FilePath -> ByteString -> (Settings, [String])
readSettings path contents = case YAML.decodeEither' contents of
  Left err -> (defaultSettings, [quoted path ++ " isn't valid YAML, so it's being ignored: " ++ show err])
  Right Null -> (defaultSettings, [])
  Right (Object o) ->
    let (folder, folderWarnings) = case KM.lookup "projects-folder" o of
          Nothing -> (Nothing, [])
          Just (String t) | not (Text.null t) -> (Just (Text.unpack t), [])
          Just _ -> (Nothing, ["ignoring the projects-folder setting, which should be a folder's path"])
        flag key def = case KM.lookup (Key.fromString key) o of
          Nothing -> (def, [])
          Just (Bool b) -> (b, [])
          Just _ -> (def, ["ignoring the " ++ key ++ " setting, which should be true or false"])
        (font, fontWarnings) = case KM.lookup "editor-font" o of
          Nothing -> (Nothing, [])
          Just (String t) | not (Text.null t) -> (Just (Text.unpack t), [])
          Just _ -> (Nothing, ["ignoring the editor-font setting, which should be a font's name"])
        (fontSize, fontSizeWarnings) = case KM.lookup "editor-font-size" o of
          Nothing -> (Nothing, [])
          Just (Number n) | n >= 4, n <= 200, n == fromIntegral (round n :: Int) -> (Just (round n), [])
          Just _ -> (Nothing, ["ignoring the editor-font-size setting, which should be a whole number of points"])
        (numbers, numberWarnings) = flag "line-numbers" False
        (emacs, emacsWarnings) = flag "emacs-keys" False
        (symbols, symbolWarnings) = flag "math-symbols" True
        (folds, foldWarnings) = flag "fold-code-blocks" False
        other = foldr KM.delete o [ "projects-folder", "editor-font", "editor-font-size"
                                  , "line-numbers", "emacs-keys", "math-symbols", "fold-code-blocks" ]
    in ( Settings folder font fontSize numbers emacs symbols folds other
       , map (("In " ++ quoted path ++ ", ") ++)
             (folderWarnings ++ fontWarnings ++ fontSizeWarnings ++ numberWarnings
              ++ emacsWarnings ++ symbolWarnings ++ foldWarnings) )
  Right _ -> (defaultSettings, [quoted path ++ " isn't a set of settings, so it's being ignored"])

-- | The settings file's contents for these settings. A setting that's
-- the same as its default is left out.
writeSettings :: Settings -> ByteString
writeSettings Settings{..} = YAML.encode $ Object $ KM.union
  (KM.fromList (catMaybes
    [ ("projects-folder",) . String . Text.pack <$> setProjectsFolder
    , ("editor-font",) . String . Text.pack <$> setEditorFont
    , ("editor-font-size",) . Number . fromIntegral <$> setEditorFontSize
    , if setLineNumbers then Just ("line-numbers", Bool True) else Nothing
    , if setEmacsKeys then Just ("emacs-keys", Bool True) else Nothing
    , if setMathSymbols then Nothing else Just ("math-symbols", Bool False)
    , if setFoldCodeBlocks then Just ("fold-code-blocks", Bool True) else Nothing
    ]))
  setOther

-- | Save the settings, by writing a new file and renaming it over the
-- old one, so that a failure part-way never leaves a damaged file.
saveSettings :: Settings -> IO ()
saveSettings settings = do
  path <- settingsFile
  createDirectoryIfMissing True (takeDirectory path)
  let tmp = path ++ ".tmp"
  BS.writeFile tmp (writeSettings settings)
  renameFile tmp path
