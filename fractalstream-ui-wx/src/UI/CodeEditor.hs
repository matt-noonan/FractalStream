module UI.CodeEditor
  ( codeEditor
  , frameIsLight
  , editorOffsetMap
  ) where

import FractalStream.Prelude hiding (get)

import Language.Parser.SourceRange (SourceRange(..))
import Data.DynamicValue
import Actor.Layout (CodeString(..))

import Graphics.UI.WX hiding (Vertical, Horizontal)
import Graphics.UI.WXCore.WxcClasses

import qualified Data.ByteString.UTF8 as Utf8
import qualified Data.ByteString as BS
import qualified Data.Map as Map

frameIsLight :: Window a -> IO Bool
frameIsLight ce = do
  col <- get ce bgcolor
  let rc :: Float = colorRed col / 255
      gc = colorGreen col / 255
      bc = colorBlue col / 255
  pure (0.299 * rc + 0.587 * gc + 0.114 * bc > 0.5)

editorOffsetMap :: String -> Map Int Int
editorOffsetMap = Map.fromList . go 0 . zip [0..]
  where
    go !tgt = \case
      [] -> []
      ((src, c) : cs) -> (src, tgt) : go (tgt + BS.length (Utf8.fromChar c)) cs

codeEditor :: Window a -> Mapped CodeString (Either (SourceRange, String) t) -> IO (StyledTextCtrl ())
codeEditor cep code = do
    stc <- styledTextCtrl cep [ clientSize := sz 100 100 ]
    styledTextCtrlSetMarginWidth stc 0 30
    styledTextCtrlSetMarginWidth stc 1 0
    styledTextCtrlSetMarginWidth stc 2 0
    -- see Style Definition at https://www.scintilla.org/ScintillaDoc.html#Styling
    isLight <- frameIsLight cep
    if isLight
      then do
        styledTextCtrlStyleSetSpec stc  0 "fore:#000000,back:#f8f8f8"
        styledTextCtrlStyleSetSpec stc  1 "fore:#000000,back:#f8a0a0"
        styledTextCtrlStyleSetSpec stc  2 "fore:#8080a0,back:#f8f8f8"
        styledTextCtrlStyleSetSpec stc 32 "fore:#000000,back:#f8f8f8"
        styledTextCtrlStyleSetSpec stc 33 "fore:#a0a098,back:#f0f0e8"
        styledTextCtrlSetCaretForeground stc (rgb 0 0 (0 :: Word8))
        styledTextCtrlSetCaretLineBackground stc (rgb 240 240 (255 :: Word8))
      else do
        styledTextCtrlStyleSetSpec stc  0 "fore:#dbdbdb,back:#14191e"
        styledTextCtrlStyleSetSpec stc  1 "fore:#dbdbdb,back:#882020"
        styledTextCtrlStyleSetSpec stc  2 "fore:#c0c0e8,back:#14191e"
        styledTextCtrlStyleSetSpec stc 32 "fore:#dbdbdb,back:#14191e"
        styledTextCtrlStyleSetSpec stc 33 "fore:#a0a0a0,back:#101020"
        styledTextCtrlSetCaretForeground stc (rgb 255 255 (255 :: Word8))
        styledTextCtrlSetCaretLineBackground stc (rgb 0x20 0x30 (0x38 :: Word8))

    styledTextCtrlStyleSetFaceName stc 0 "Monaco"
    styledTextCtrlStyleSetFaceName stc 1 "Monaco"
    styledTextCtrlStyleSetFontAttr stc 2 12 "Monaco" False True False
    -- Set the minimum size, or else Scintilla will default to 2000 pixels(!)
    styledTextCtrlSetScrollWidth stc 10
    --styledTextCtrlSetCaretLineVisible stc True

    styledTextCtrlSetUseTabs stc False
    styledTextCtrlSetTabWidth stc 4
    styledTextCtrlSetIndent stc 4
    styledTextCtrlSetTabIndents stc True
    styledTextCtrlSetBackSpaceUnIndents stc True
    styledTextCtrlSetWrapMode stc 1
    --styledTextCtrlSetWrapVisualFlags stc 2
    --styledTextCtrlSetIndentationGuides stc True -- should take an integer?
    --styledTextCtrlSetViewWhiteSpace stc 3
    CodeString txt <- getDynamic (source code)
    styledTextCtrlSetText stc txt
    pure stc
