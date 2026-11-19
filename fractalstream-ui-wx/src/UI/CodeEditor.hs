module UI.CodeEditor
  ( codeEditor
  , codeEditorWithText
  , updateFoldLevels
  , refreshFolds
  , markError
  , clearError
  , showErrorNote
  , markErrorLine
  , errorNoteLayout
  , frameIsLight
  , editorOffsetMap
  ) where

import FractalStream.Prelude hiding (get)

import Language.Parser.SourceRange (SourceRange(..))
import Data.DynamicValue
import Actor.Layout (CodeString(..))
import UI.EditorKeys
import UI.SymbolEntry
import UI.EditorStyle (setupStyles, showLineNumbers, errorNoteStyle, errorColor)
import Language.Editor (indentUnit, visibleSpan)
import UI.AppSettings (appSettings)
import FractalStream.Settings (Settings(..))

import Graphics.UI.WX hiding (Vertical, Horizontal, when)
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.Events (stcOnSTCEvent, EventSTC(..))

import Data.Bits ((.|.), (.&.))
import Data.List (zipWith4)
import Data.IORef (newIORef)
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

-- | An editor for a script, with what clicking the margin's mark of an
-- error does (given the line clicked).
codeEditor :: Window a -> Mapped CodeString (Either (SourceRange, String) t) -> (Int -> IO ())
           -> IO (StyledTextCtrl ())
codeEditor cep code onErrorMarkClick = do
  CodeString txt <- getDynamic (source code)
  codeEditorWithText cep txt onErrorMarkClick

-- | The editor's margins: 0 is line numbers, 1 marks the line with an
-- error, and 2 is for folding.
errorMarginIndex, foldMarginIndex :: Int
errorMarginIndex = 1
foldMarginIndex = 2

-- | The marker that marks the line with an error (in the error margin;
-- the fold margin's are 25 to 31).
errorMarker :: Int
errorMarker = 0

-- | The styled-text-control setup shared by every code\/script editor in
-- this UI, taking the initial text directly rather than through a
-- 'Mapped' -- for a caller whose own reactivity can't be expressed as a
-- 'Mapped' (its recompute step is an 'IO' action, e.g. allocating fresh
-- 'Variable's, not a pure function of the source text).
codeEditorWithText :: Window a -> String -> (Int -> IO ()) -> IO (StyledTextCtrl ())
codeEditorWithText cep txt onErrorMarkClick = do
    stc <- styledTextCtrl cep [ clientSize := sz 100 100 ]
    -- see Style Definition at https://www.scintilla.org/ScintillaDoc.html#Styling
    isLight <- frameIsLight cep
    -- each class of span its own style ("UI.EditorStyle")
    settings <- getDynamic appSettings
    setupStyles stc isLight settings
    showLineNumbers stc (setLineNumbers settings)
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

    -- The error margin: a red dot on the line with an error, which can
    -- be clicked (to show its message, or hide it: see 'showErrorNote').
    let (er, eg, eb) = errorColor isLight
    styledTextCtrlSetMarginType stc errorMarginIndex wxSTC_MARGIN_SYMBOL
    styledTextCtrlSetMarginMask stc errorMarginIndex (2 ^ errorMarker)
    styledTextCtrlSetMarginSensitive stc errorMarginIndex True
    styledTextCtrlSetMarginWidth stc errorMarginIndex 12
    styledTextCtrlMarkerDefine stc errorMarker wxSTC_MARK_CIRCLE (rgb er eg eb) (rgb er eg eb)
    -- (an arrow, not the reversed one Scintilla's margins have, since
    -- this isn't for selecting lines)
    void (styledTextCtrlSendMsg stc sciSetMarginCursorN errorMarginIndex sciCursorArrow)
    -- An error's message goes in a box under its line.
    void (styledTextCtrlSendMsg stc sciAnnotationSetVisible sciAnnotationBoxed 0)

    -- symbols typed by name ("UI.SymbolEntry")
    symbols <- symbolEntry stc isLight
    -- where the cursor was ("UI.EditorKeys.keepCursorOnCharacters")
    lastCursor <- newIORef 0

    stcOnSTCEvent stc $ \case
      STCMarginClick _ _ _ pos clickedMargin | clickedMargin == foldMarginIndex -> do
        lineNum <- styledTextCtrlLineFromPosition stc pos
        styledTextCtrlToggleFold stc lineNum
      STCMarginClick _ _ _ pos clickedMargin | clickedMargin == errorMarginIndex ->
        styledTextCtrlLineFromPosition stc pos >>= onErrorMarkClick
      STCCharAdded c _ -> symbolCharAdded symbols c
      STCModified pos modType _ len _ _ _ _ -> symbolTextModified symbols pos modType len
      STCUpdateUI -> do
        symbolUpdateUI symbols
        keepCursorOnCharacters stc lastCursor
      _ -> pure ()
    -- (a caller that handles focus too has to keep this, with `:~`)
    set stc [ on focus := \gained -> do
                unless gained (void (cancelSymbol symbols))
                propagateEvent ]

    -- (on platforms where they're a preference, it's checked on each
    -- key, so changing it applies to editors that are already open)
    -- errors are underlined with a red squiggle
    styledTextCtrlIndicatorSetStyle stc errorIndicator sciIndicSquigglePixmap
    styledTextCtrlIndicatorSetForeground stc errorIndicator (rgb er eg eb)

    handleEditorKeys stc symbols $ if emacsKeysByDefault then pure True
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
--
-- An error in a line is underlined in that line, over whole characters
-- that show ("Language.Editor.visibleSpan"): Scintilla draws the squiggle
-- as an image as wide as what it underlines, and an image no wider than
-- nothing (a combining mark on nothing) fails an assertion in wx, whose
-- dialog can't be shown while the editor is drawn, which hangs the app.
markError :: StyledTextCtrl a -> Int -> Int -> IO ()
markError stc from len = do
  clearError stc
  docLen <- styledTextCtrlGetLength stc
  when (docLen > 0) $ do
    let start' = max 0 (min from (docLen - 1))
        len' = max 1 (min len (docLen - start'))
    lineNum <- styledTextCtrlLineFromPosition stc start'
    lineStart <- styledTextCtrlPositionFromLine stc lineNum
    lineEnd <- styledTextCtrlGetLineEndPosition stc lineNum
    span' <- if start' >= lineEnd
      -- (at the end of a line: the line break, as it is)
      then pure (Just (start', len'))
      else do
        lineText <- styledTextCtrlGetTextRange stc lineStart lineEnd
        let bytes = BS.length . Utf8.fromString
            -- (character offsets in the line, from byte positions)
            charAt pos = length (Utf8.toString (BS.take (pos - lineStart) (Utf8.fromString lineText)))
            byteAt i = lineStart + bytes (take i lineText)
        pure $ visibleSpan lineText (charAt start') (charAt (min lineEnd (start' + len'))) <&> \(i, j) ->
          (byteAt i, byteAt j - byteAt i)
    forM_ span' $ \(s, l) -> do
      void (styledTextCtrlSendMsg stc sciSetIndicatorCurrent errorIndicator 0)
      void (styledTextCtrlSendMsg stc sciIndicatorFillRange s l)

-- | Remove the error underline, if there is one.
clearError :: StyledTextCtrl a -> IO ()
clearError stc = do
  docLen <- styledTextCtrlGetLength stc
  void (styledTextCtrlSendMsg stc sciSetIndicatorCurrent errorIndicator 0)
  void (styledTextCtrlSendMsg stc sciIndicatorClearRange 0 docLen)

-- | Show an error's note under a line (its lines of text: see
-- "Language.Editor.errorNote"), or no note. There's only ever one note:
-- this replaces any other.
--
-- (Scintilla's annotations: drawn under their line, in the error
-- note's style, moving with the line as the text is edited.)
showErrorNote :: StyledTextCtrl a -> Maybe (Int, [String]) -> IO ()
showErrorNote stc note = do
  void (styledTextCtrlSendMsg stc sciAnnotationClearAll 0 0)
  forM_ note $ \(lineNum, noteLines) -> do
    void (styledTextCtrlSendMsgString stc sciAnnotationSetText lineNum (intercalate "\n" noteLines))
    void (styledTextCtrlSendMsg stc sciAnnotationSetStyle lineNum errorNoteStyle)

-- | Mark the line with an error in the margin, or no line.
markErrorLine :: StyledTextCtrl a -> Maybe Int -> IO ()
markErrorLine stc lineNum = do
  styledTextCtrlMarkerDeleteAll stc errorMarker
  forM_ lineNum $ \l -> void (styledTextCtrlMarkerAdd stc l errorMarker)

-- | How to lay out an error's note under a line, given where the error
-- is in it (byte positions, the end not included), if anywhere: how many
-- columns wide the note can be, to fit in the editor as it is now (a
-- note doesn't wrap, as the text does), and where its carets go under
-- the error, as how many spaces go before them and how many there are
-- (see "Language.Editor.errorNote").
--
-- Scintilla draws a boxed note from the line's indentation, and the
-- font may not be monospaced, so that's found from where they are on
-- the screen; no carets, if the error isn't in the first row of a line
-- that wraps.
errorNoteLayout :: StyledTextCtrl a -> Int -> Maybe (Int, Int) -> IO (Int, Maybe (Int, Int))
errorNoteLayout stc lineNum span' = do
  indentPos <- styledTextCtrlGetLineIndentPosition stc lineNum
  let pointAt pos = (,) <$> styledTextCtrlSendMsg stc sciPointXFromPosition 0 pos
                        <*> styledTextCtrlSendMsg stc sciPointYFromPosition 0 pos
      widthOf t = max 1 <$> styledTextCtrlTextWidth stc errorNoteStyle t
  (x0, y0) <- pointAt indentPos
  Size w _ <- get stc clientSize
  digit <- widthOf "0"
  spaceWidth <- widthOf " "
  caretWidth <- widthOf "^"
  let width = max 20 ((w - x0) `div` digit - 3)
      near :: Int -> Int -> Int
      near a b = round (fromIntegral a / fromIntegral b :: Double)
  carets <- case span' of
    Nothing -> pure Nothing
    Just (from, to) -> do
      (xs, ys) <- pointAt from
      (xe, ye) <- pointAt to
      pure $ if ys /= y0 || ye /= y0 then Nothing
             else Just (near (xs - x0) spaceWidth, max 1 (near (xe - xs) caretWidth))
  pure (width, carets)

-- Scintilla's numbers for these (from Scintilla.h)
sciAnnotationSetText, sciAnnotationSetStyle, sciAnnotationClearAll, sciAnnotationSetVisible :: Int
sciAnnotationSetText = 2540
sciAnnotationSetStyle = 2542
sciAnnotationClearAll = 2547
sciAnnotationSetVisible = 2548
sciPointXFromPosition, sciPointYFromPosition :: Int
sciPointXFromPosition = 2164
sciPointYFromPosition = 2165
sciAnnotationBoxed, sciSetMarginCursorN, sciCursorArrow :: Int
sciAnnotationBoxed = 2
sciSetMarginCursorN = 2248
sciCursorArrow = 2
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
    --
    -- Scintilla folds a blank line (one with the whiteflag) away with
    -- the block before it, whatever its level. Blank lines between
    -- stanzas, or between a stanza's top-level parts (tool handlers),
    -- don't get it: they stay out of the fold before them, so folded
    -- stanzas and handlers keep the space between them, rather than
    -- stacking up.
    combine l rawIndent indent nextIndent =
      (wxSTC_FOLDLEVELBASE + indent)
      .|. (if isNothing rawIndent && indent > stanzaBodyWeight then wxSTC_FOLDLEVELWHITEFLAG else 0)
      .|. (if nextIndent > indent && folds l then wxSTC_FOLDLEVELHEADERFLAG else 0)

    -- which lines can be folded: stanza headers and tool handlers' `on`
    -- lines (both at the left margin), and others only if asked for
    folds l = codeBlocks || "@" `isPrefixOf` l || "on " `isPrefixOf` l
