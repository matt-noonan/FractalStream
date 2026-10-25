module UI.CodeEditor
  ( codeEditor
  , codeEditorWithText
  , updateFoldLevels
  , stanzaHeaderStyle
  , frameIsLight
  , editorOffsetMap
  ) where

import FractalStream.Prelude hiding (get)

import Language.Parser.SourceRange (SourceRange(..))
import Data.DynamicValue
import Actor.Layout (CodeString(..))

import Graphics.UI.WX hiding (Vertical, Horizontal)
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.Events (stcOnSTCEvent, EventSTC(..))

import Data.Bits ((.|.))
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
  CodeString txt <- getDynamic (source code)
  codeEditorWithText cep txt

-- | The style number for a unified script's @\@setup@\/@\@config@\/
-- @\@viewer@\/@\@tool@ stanza-header lines -- a caller that wants them
-- styled distinctly (a single viewer's own code never has any, so only
-- the whole-project script editor uses this) applies it itself, since
-- recognizing a stanza header is specific to that document shape, not
-- something this generic editor setup should know about.
stanzaHeaderStyle :: Int
stanzaHeaderStyle = 3

-- | The margin used for the fold margin (0 is line numbers; 1 is
-- unused).
foldMarginIndex :: Int
foldMarginIndex = 2

-- | The styled-text-control setup shared by every code\/script editor in
-- this UI, taking the initial text directly rather than through a
-- 'Mapped' -- for a caller whose own reactivity can't be expressed as a
-- 'Mapped' (its recompute step is an 'IO' action, e.g. allocating fresh
-- 'Variable's, not a pure function of the source text).
codeEditorWithText :: Window a -> String -> IO (StyledTextCtrl ())
codeEditorWithText cep txt = do
    stc <- styledTextCtrl cep [ clientSize := sz 100 100 ]
    styledTextCtrlSetMarginWidth stc 0 30
    styledTextCtrlSetMarginWidth stc 1 0
    -- see Style Definition at https://www.scintilla.org/ScintillaDoc.html#Styling
    isLight <- frameIsLight cep
    if isLight
      then do
        styledTextCtrlStyleSetSpec stc  0 "fore:#000000,back:#f8f8f8"
        styledTextCtrlStyleSetSpec stc  1 "fore:#000000,back:#f8a0a0"
        styledTextCtrlStyleSetSpec stc  2 "fore:#8080a0,back:#f8f8f8"
        styledTextCtrlStyleSetSpec stc  3 "fore:#403060,back:#e8e0f8,italic"
        styledTextCtrlStyleSetSpec stc 32 "fore:#000000,back:#f8f8f8"
        styledTextCtrlStyleSetSpec stc 33 "fore:#a0a098,back:#f0f0e8"
        styledTextCtrlSetCaretForeground stc (rgb 0 0 (0 :: Word8))
        styledTextCtrlSetCaretLineBackground stc (rgb 240 240 (255 :: Word8))
        styledTextCtrlSetFoldMarginColour stc True (rgb 0xd0 0xd0 (0xd0 :: Word8))
        styledTextCtrlSetFoldMarginHiColour stc True (rgb 0xff 0xff (0xff :: Word8))
        defineFoldMarkers stc (rgb 0xf8 0xf8 (0xf8 :: Word8)) (rgb 0x60 0x60 (0x60 :: Word8))
      else do
        styledTextCtrlStyleSetSpec stc  0 "fore:#dbdbdb,back:#14191e"
        styledTextCtrlStyleSetSpec stc  1 "fore:#dbdbdb,back:#882020"
        styledTextCtrlStyleSetSpec stc  2 "fore:#c0c0e8,back:#14191e"
        styledTextCtrlStyleSetSpec stc  3 "fore:#c0c0e8,back:#262040,italic"
        styledTextCtrlStyleSetSpec stc 32 "fore:#dbdbdb,back:#14191e"
        styledTextCtrlStyleSetSpec stc 33 "fore:#a0a0a0,back:#101020"
        styledTextCtrlSetCaretForeground stc (rgb 255 255 (255 :: Word8))
        styledTextCtrlSetCaretLineBackground stc (rgb 0x20 0x30 (0x38 :: Word8))
        styledTextCtrlSetFoldMarginColour stc True (rgb 0x14 0x19 (0x1e :: Word8))
        styledTextCtrlSetFoldMarginHiColour stc True (rgb 0x14 0x19 (0x1e :: Word8))
        defineFoldMarkers stc (rgb 0x14 0x19 (0x1e :: Word8)) (rgb 0xa0 0xa0 (0xa0 :: Word8))

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

    -- Fold margin: this language is indentation-structured (like
    -- Python), with no Scintilla lexer of its own to compute fold
    -- levels automatically, so `updateFoldLevels` derives them from the
    -- text's own indentation and the caller re-runs it on every edit.
    styledTextCtrlSetMarginType stc foldMarginIndex wxSTC_MARGIN_SYMBOL
    styledTextCtrlSetMarginMask stc foldMarginIndex wxSTC_MASK_FOLDERS
    styledTextCtrlSetMarginSensitive stc foldMarginIndex True
    styledTextCtrlSetMarginWidth stc foldMarginIndex 16
    styledTextCtrlSetFoldFlags stc wxSTC_FOLDFLAG_LINEBEFORE_CONTRACTED
    stcOnSTCEvent stc $ \case
      STCMarginClick _ _ _ pos clickedMargin | clickedMargin == foldMarginIndex -> do
        lineNum <- styledTextCtrlLineFromPosition stc pos
        styledTextCtrlToggleFold stc lineNum
      _ -> pure ()

    styledTextCtrlSetText stc txt
    updateFoldLevels stc txt
    pure stc

-- | Define the standard 7-marker Scintilla fold-margin symbol set
-- (+\/- boxes with connector lines): @fillColor@ is the inside of each
-- box (typically matching the margin's own background, so it blends
-- in), @lineColor@ is the box outline\/connector-line color.
defineFoldMarkers :: StyledTextCtrl a -> Color -> Color -> IO ()
defineFoldMarkers stc fillColor lineColor = mapM_ defineOne
  [ (wxSTC_MARKNUM_FOLDEROPEN,    wxSTC_MARK_BOXMINUS)
  , (wxSTC_MARKNUM_FOLDER,        wxSTC_MARK_BOXPLUS)
  , (wxSTC_MARKNUM_FOLDERSUB,     wxSTC_MARK_VLINE)
  , (wxSTC_MARKNUM_FOLDERTAIL,    wxSTC_MARK_LCORNER)
  , (wxSTC_MARKNUM_FOLDEREND,     wxSTC_MARK_BOXPLUSCONNECTED)
  , (wxSTC_MARKNUM_FOLDEROPENMID, wxSTC_MARK_BOXMINUSCONNECTED)
  , (wxSTC_MARKNUM_FOLDERMIDTAIL, wxSTC_MARK_TCORNER)
  ]
  where
    defineOne (markNum, markSymbol) =
      styledTextCtrlMarkerDefine stc markNum markSymbol lineColor fillColor

-- | Recompute every line's Scintilla fold level from the given text's
-- own indentation -- the "Python mode" convention most editors use for
-- whitespace-structured languages, since this language has no
-- Scintilla lexer of its own to do it automatically. A blank line
-- inherits the next non-blank line's indent (so it never looks like a
-- dedent on its own), and a line is a fold header exactly when the
-- next non-blank line is indented deeper than it is. The caller is
-- responsible for re-running this on every edit, not just once at
-- construction.
updateFoldLevels :: StyledTextCtrl a -> String -> IO ()
updateFoldLevels stc txt =
  forM_ (zip [0 ..] (foldLevelsForText txt)) $ \(lineNum, level) ->
    styledTextCtrlSetFoldLevel stc lineNum level

foldLevelsForText :: String -> [Int]
foldLevelsForText txt =
  zipWith3 combine rawIndents effectiveIndents (drop 1 effectiveIndents ++ [0])
  where
    rawIndents = map indentOf (lines txt)
    indentOf l
      | all (`elem` (" \t" :: String)) l = Nothing
      -- A unified script's stanza header (`@setup`/`@config`/
      -- `@viewer`/`@tool`) is always flush-left; everything else --
      -- every stanza's own body, and every line of a script with no
      -- stanzas at all (a single viewer's own code) -- sits a full
      -- `stanzaBodyWeight` deeper than any real indentation could
      -- reach. Without this, a stanza header can't be a fold point at
      -- all: its header line and its body both sit at indent 0 (a
      -- stanza's body is flush-left, not indented under its header),
      -- so plain indentation gives Scintilla no signal that collapsing
      -- the header should hide the whole stanza. This still leaves
      -- every nested if\/while\/on block's own folding completely
      -- unaffected, just uniformly one level deeper under it.
      | "@" `isPrefixOf` l = Just (length (takeWhile (== ' ') l))
      | otherwise          = Just (stanzaBodyWeight + length (takeWhile (== ' ') l))

    stanzaBodyWeight = 1000

    -- A blank line's effective indent is whatever the next non-blank
    -- line's indent turns out to be (0 if there isn't one) -- computed
    -- right-to-left, each element only ever looking at the one after it.
    effectiveIndents = foldr nextIndent [] rawIndents
      where
        nextIndent (Just i)  acc = i : acc
        nextIndent Nothing   acc = headOr 0 acc : acc
        headOr d []      = d
        headOr _ (x : _) = x

    -- The header check compares raw indent, not the finished level
    -- value below -- the whiteflag bit it can carry is far larger than
    -- any real indent difference and would otherwise make any blank
    -- line look like a header no matter its neighbours' actual depth.
    combine rawIndent indent nextIndent =
      (wxSTC_FOLDLEVELBASE + indent)
      .|. (if isNothing rawIndent then wxSTC_FOLDLEVELWHITEFLAG else 0)
      .|. (if nextIndent > indent then wxSTC_FOLDLEVELHEADERFLAG else 0)
