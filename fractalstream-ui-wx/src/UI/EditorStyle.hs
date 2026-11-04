-- | How code editors color each class of span ("Language.Editor.Highlight"):
-- a Scintilla style for each class, with colors for light and dark
-- windows. Every class has a style of its own, though some share colors
-- by default, and any of them can be changed in the settings
-- (@editor-colors-light@ and @editor-colors-dark@).
module UI.EditorStyle
  ( styleFor
  , setupStyles
  , showLineNumbers
  , Style(..)
  , classStyle
  , defaultClassStyle
  , editorBackground
  ) where

import FractalStream.Prelude hiding (get)

import qualified Data.Map as Map

import FractalStream.Settings
import Language.Editor.Highlight (Highlight(..), highlightName)

import Graphics.UI.WXCore.WxcClasses

-- | The Scintilla style for a class. (0 is plain text; 32 and up are
-- Scintilla's own: the default style, line numbers, ...)
styleFor :: Highlight -> Int
styleFor h = 1 + fromEnum h

-- | How a class is shown: its color, its background (the editor's, if
-- 'Nothing'), and whether it's bold, italic, and underlined.
data Style = Style
  { stColor      :: String
  , stBackground :: Maybe String
  , stBold       :: Bool
  , stItalic     :: Bool
  , stUnderline  :: Bool
  }
  deriving (Eq, Show)

-- | The editor's text color and background, in a light window or a dark one.
editorText, editorBackground :: Bool -> String
editorText isLight = if isLight then "#000000" else "#dbdbdb"
editorBackground isLight = if isLight then "#f8f8f8" else "#14191e"

-- | How a class is shown, in a light window or a dark one, with the
-- settings' changes to it.
classStyle :: Bool -> Settings -> Highlight -> Style
classStyle isLight settings h =
  let changes = Map.findWithDefault noClassStyle (highlightName h)
                  ((if isLight then setEditorColorsLight else setEditorColorsDark) settings)
      Style{..} = defaultClassStyle isLight h
  in Style { stColor      = fromMaybe stColor (csColor changes)
           , stBackground = maybe stBackground Just (csBackground changes)
           , stBold       = fromMaybe stBold (csBold changes)
           , stItalic     = fromMaybe stItalic (csItalic changes)
           , stUnderline  = fromMaybe stUnderline (csUnderline changes) }

-- | Set up an editor's styles, for a light window or a dark one, with
-- the settings' font and colors: every style in the same font, and each
-- class's colors on top of that. This can be done again, when they
-- change.
setupStyles :: StyledTextCtrl a -> Bool -> Settings -> IO ()
setupStyles stc isLight settings = do
  let lineNumbers = if isLight then "fore:#a0a098,back:#f0f0e8" else "fore:#a0a0a0,back:#101020"
  -- the default style, copied to every other (so they all share its font)
  styledTextCtrlStyleSetSpec stc 32 ("fore:" ++ editorText isLight ++ ",back:" ++ editorBackground isLight)
  styledTextCtrlStyleSetFaceName stc 32 (editorFont settings)
  forM_ (setEditorFontSize settings) (styledTextCtrlStyleSetSize stc 32)
  styledTextCtrlStyleClearAll stc
  styledTextCtrlStyleSetSpec stc 33 lineNumbers
  forM_ [minBound .. maxBound] $ \h -> do
    let Style{..} = classStyle isLight settings h
        n = styleFor h
    styledTextCtrlStyleSetSpec stc n
      ("fore:" ++ stColor ++ ",back:" ++ fromMaybe (editorBackground isLight) stBackground)
    styledTextCtrlStyleSetBold stc n stBold
    styledTextCtrlStyleSetItalic stc n stItalic
    styledTextCtrlStyleSetUnderline stc n stUnderline

-- | Show line numbers in an editor's margin, or not. (Its width depends
-- on the font: this goes after 'setupStyles'.)
showLineNumbers :: StyledTextCtrl a -> Bool -> IO ()
showLineNumbers stc on' = do
  -- wide enough for line numbers up to 999, however big the font
  numberWidth <- styledTextCtrlTextWidth stc 33 "_999"
  styledTextCtrlSetMarginWidth stc 0 (if on' then numberWidth + 4 else 0)

-- | A class's own style, in a light window or a dark one.
defaultClassStyle :: Bool -> Highlight -> Style
defaultClassStyle isLight h = case h of
  Keyword         -> (plain "#7a3e9d" "#c792ea") { stBold = True }
  BuiltinFunction -> plain "#1f5fbf" "#82aaff"
  BuiltinValue    -> plain "#a35200" "#f78c6c"
  TypeName        -> plain "#0a7a6a" "#7fdbca"
  ColorName       -> plain "#a35200" "#f78c6c"
  NumberLiteral   -> plain "#a35200" "#f78c6c"
  StringLiteral   -> plain "#2e7d32" "#c3e88d"
  Comment         -> (plain "#8080a0" "#8a8fb8") { stItalic = True }
  Operator        -> plain "#606060" "#89ddff"
  StanzaHeader    -> (plain "#403060" "#c0c0e8")
                       { stItalic = True, stBackground = Just (if isLight then "#e8e0f8" else "#262040") }
  Parameter       -> plain "#9a2a5a" "#ffcb6b"
  Variable        -> plain (editorText True) (editorText False)
  Function        -> plain "#1f5fbf" "#82aaff"
  CodeBlock       -> plain "#7a3e9d" "#c792ea"
  Undefined       -> (plain (editorText True) (editorText False)) { stItalic = True }
  where
    plain light dark = Style (if isLight then light else dark) Nothing False False False
