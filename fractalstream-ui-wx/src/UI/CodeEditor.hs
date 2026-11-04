module UI.CodeEditor
  ( codeEditor
  , codeEditorWithText
  , updateFoldLevels
  , refreshFolds
  , markError
  , clearError
  , frameIsLight
  , editorOffsetMap
  ) where

import FractalStream.Prelude hiding (get)

import Language.Parser.SourceRange (SourceRange(..))
import Data.DynamicValue
import Actor.Layout (CodeString(..))
import UI.EditorKeys
import UI.EditorStyle (setupStyles)
import Language.Editor (indentUnit)
import UI.AppSettings (appSettings)
import FractalStream.Settings (Settings(..))

import Graphics.UI.WX hiding (Vertical, Horizontal, when)
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.Events (stcOnSTCEvent, EventSTC(..))

import Data.Bits ((.|.), (.&.))
import Data.List (zipWith4)
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
    -- each class of span its own style ("UI.EditorStyle")
    setupStyles stc isLight
    if isLight
      then do
        styledTextCtrlSetCaretForeground stc (rgb 0 0 (0 :: Word8))
        styledTextCtrlSetCaretLineBackground stc (rgb 240 240 (255 :: Word8))
        styledTextCtrlSetFoldMarginColour stc True (rgb 0xd0 0xd0 (0xd0 :: Word8))
        styledTextCtrlSetFoldMarginHiColour stc True (rgb 0xff 0xff (0xff :: Word8))
        defineFoldMarkers stc (rgb 0xf8 0xf8 (0xf8 :: Word8)) (rgb 0x60 0x60 (0x60 :: Word8))
      else do
        styledTextCtrlSetCaretForeground stc (rgb 255 255 (255 :: Word8))
        styledTextCtrlSetCaretLineBackground stc (rgb 0x20 0x30 (0x38 :: Word8))
        styledTextCtrlSetFoldMarginColour stc True (rgb 0x14 0x19 (0x1e :: Word8))
        styledTextCtrlSetFoldMarginHiColour stc True (rgb 0x14 0x19 (0x1e :: Word8))
        defineFoldMarkers stc (rgb 0x14 0x19 (0x1e :: Word8)) (rgb 0xa0 0xa0 (0xa0 :: Word8))

    -- Set the minimum size, or else Scintilla will default to 2000 pixels(!)
    styledTextCtrlSetScrollWidth stc 10
    --styledTextCtrlSetCaretLineVisible stc True

    styledTextCtrlSetUseTabs stc False
    styledTextCtrlSetTabWidth stc indentUnit
    styledTextCtrlSetIndent stc indentUnit
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

    -- (on platforms where they're a preference, it's checked on each
    -- key, so changing it applies to editors that are already open)
    -- errors are underlined with a red squiggle
    styledTextCtrlIndicatorSetStyle stc errorIndicator sciIndicSquigglePixmap
    styledTextCtrlIndicatorSetForeground stc errorIndicator
      (if isLight then rgb 0xd0 0x20 (0x20 :: Word8) else rgb 0xff 0x60 (0x60 :: Word8))

    handleEditorKeys stc $ if emacsKeysByDefault then pure True
                          else setEmacsKeys <$> getDynamic appSettings

    styledTextCtrlSetText stc txt
    updateFoldLevels stc txt
    pure stc

-- | The indicator that underlines errors: the first one Scintilla leaves
-- for containers (0 to 7 are for its lexers).
errorIndicator :: Int
errorIndicator = 8

-- | Underline the error from byte position @from@, @len@ bytes long
-- (at least one character, so an error at the end still shows), in
-- place of any error underlined before.
--
-- Indicators are set with Scintilla's own messages, which wxHaskell has
-- no functions for ("Graphics.UI.WXCore"'s indicator functions are for
-- the older style-bit indicators, which Scintilla no longer draws).
markError :: StyledTextCtrl a -> Int -> Int -> IO ()
markError stc from len = do
  clearError stc
  docLen <- styledTextCtrlGetLength stc
  when (docLen > 0) $ do
    let start' = max 0 (min from (docLen - 1))
    void (styledTextCtrlSendMsg stc sciSetIndicatorCurrent errorIndicator 0)
    void (styledTextCtrlSendMsg stc sciIndicatorFillRange start' (max 1 (min len (docLen - start'))))

-- | Remove the error underline, if there is one.
clearError :: StyledTextCtrl a -> IO ()
clearError stc = do
  docLen <- styledTextCtrlGetLength stc
  void (styledTextCtrlSendMsg stc sciSetIndicatorCurrent errorIndicator 0)
  void (styledTextCtrlSendMsg stc sciIndicatorClearRange 0 docLen)

-- Scintilla's numbers for these (from Scintilla.h)
sciSetIndicatorCurrent, sciIndicatorFillRange, sciIndicatorClearRange, sciIndicSquigglePixmap :: Int
sciSetIndicatorCurrent = 2500
sciIndicatorFillRange = 2504
sciIndicatorClearRange = 2505
sciIndicSquigglePixmap = 13  -- INDIC_SQUIGGLEPIXMAP: a squiggle, drawn quickly

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
--
-- Stanzas and tool handlers (@on click:@, ...) always fold; other blocks
-- of code only with the @fold-code-blocks@ setting. Changing that setting
-- applies to editors already open ('refreshFolds').
updateFoldLevels :: StyledTextCtrl a -> String -> IO ()
updateFoldLevels stc txt = do
  codeBlocks <- setFoldCodeBlocks <$> getDynamic appSettings
  forM_ (zip [0 ..] (foldLevelsForText codeBlocks txt)) $ \(lineNum, level) ->
    styledTextCtrlSetFoldLevel stc lineNum level

-- | Recompute an editor's folds, after the setting for which blocks fold
-- has changed. Folded blocks are opened first: one that can't be folded
-- any more would otherwise stay hidden, with no fold box to open it.
refreshFolds :: StyledTextCtrl a -> IO ()
refreshFolds stc = do
  lineCount <- styledTextCtrlGetLineCount stc
  forM_ [0 .. lineCount - 1] $ \lineNum -> do
    level <- styledTextCtrlGetFoldLevel stc lineNum
    expanded <- styledTextCtrlGetFoldExpanded stc lineNum
    when (level .&. wxSTC_FOLDLEVELHEADERFLAG /= 0 && not expanded) $
      styledTextCtrlToggleFold stc lineNum
  styledTextCtrlGetText stc >>= updateFoldLevels stc

foldLevelsForText :: Bool -> String -> [Int]
foldLevelsForText codeBlocks txt =
  zipWith4 combine (lines txt) rawIndents effectiveIndents (drop 1 effectiveIndents ++ [0])
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
    combine l rawIndent indent nextIndent =
      (wxSTC_FOLDLEVELBASE + indent)
      .|. (if isNothing rawIndent then wxSTC_FOLDLEVELWHITEFLAG else 0)
      .|. (if nextIndent > indent && folds l then wxSTC_FOLDLEVELHEADERFLAG else 0)

    -- which lines can be folded: stanza headers and tool handlers' `on`
    -- lines (both at the left margin), and others only if asked for
    folds l = codeBlocks || "@" `isPrefixOf` l || "on " `isPrefixOf` l
