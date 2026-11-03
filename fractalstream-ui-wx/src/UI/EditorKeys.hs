-- | Keys in a code editor that do more than Scintilla does by itself:
--
-- * Return starts the new line indented as the language expects: as far
--   as the line it breaks, and a level further after a line ending in
--   @:@ ("Language.Editor"). (Backspace at the start of a line's code
--   takes off a level, which Scintilla does itself, given the indent
--   size.)
--
-- * Pasting (Command-V on macOS, Control-V elsewhere) moves lines of
--   code to the indentation they're pasted at, keeping their indentation
--   relative to each other.
--
-- * Command-\/ (Control-\/ elsewhere) comments out the selected lines,
--   or the cursor's line, or uncomments them if they're all comments; on
--   a blank line, it starts a comment.
--
-- * Emacs-style movement keys: Control-A and Control-E for the start and
--   end of the line, and so on. On macOS these are always on: the
--   standard shortcuts there are on Command, so the Control key is
--   otherwise unused (and without these, a Control-letter would type an
--   invisible control character). Elsewhere, Control is the standard
--   shortcut key (Control-A selects everything, Control-V pastes, ...),
--   so they're a preference, off by default.
module UI.EditorKeys
  ( emacsKeysByDefault
  , handleEditorKeys
  ) where

import FractalStream.Prelude hiding (get)

import Graphics.UI.WX hiding (Vertical, Horizontal, when)
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.Events (windowOnKeyDown)
import System.Info (os)
import Data.Char (chr, ord, toLower)
import Language.Editor (newLineIndent, pasteIndented, toggleComments)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as Utf8

-- | Whether a platform has emacs keys without asking for them.
emacsKeysByDefault :: Bool
emacsKeysByDefault = os == "darwin"

-- | Handle these keys in an editor, with emacs keys while @emacsKeys@
-- says to: each is done here and goes no further; any other key is
-- handled as usual.
handleEditorKeys :: StyledTextCtrl a -> IO Bool -> IO ()
handleEditorKeys stc emacsKeys = do
  -- Return is handled as it goes down (Scintilla breaks the line then,
  -- and no character event follows); the emacs keys as the characters
  -- they type, which is how Control-letters arrive on macOS.
  windowOnKeyDown stc $ \k -> do
    emacs <- emacsKeys
    case keyKey k of
      KeyReturn | isPlain (keyModifiers k) -> newLine stc
      KeyChar c
        -- (with emacs keys on where Control is the shortcut key,
        -- Control-V moves down a page instead)
        | toLower c == 'v', isShortcut (keyModifiers k), not (emacs && not emacsKeysByDefault) ->
            pasteFittingIndentation stc
        | c == '/', isShortcut (keyModifiers k) -> toggleCommentLines stc
      -- (on macOS, the slash key goes down as the keypad's divide key)
      KeyDivide | isShortcut (keyModifiers k) -> toggleCommentLines stc
      _ -> propagateEvent
  set stc [ on keyboard := \k -> do
    emacs <- emacsKeys
    case emacsCommand k of
      Just action | emacs -> action stc
      _ -> propagateEvent ]
  where
    isPlain m = not (altDown m || shiftDown m || controlDown m || metaDown m)
    -- the shortcut key alone: Command on macOS (which wx calls
    -- "control" there), Control elsewhere
    isShortcut m = controlDown m && not (altDown m || shiftDown m || metaDown m)

-- | Paste what's on the clipboard over the selection, re-indented to fit
-- where it goes ("Language.Editor.pasteIndented"); or, if the clipboard
-- has no text, paste as usual.
pasteFittingIndentation :: StyledTextCtrl a -> IO ()
pasteFittingIndentation stc = clipboardText >>= \case
  Nothing -> styledTextCtrlPaste stc
  Just pasted -> do
    from <- styledTextCtrlGetSelectionStart stc
    lineNum <- styledTextCtrlLineFromPosition stc from
    lineStart <- styledTextCtrlPositionFromLine stc lineNum
    before <- styledTextCtrlGetTextRange stc lineStart from
    styledTextCtrlBeginUndoAction stc
    styledTextCtrlReplaceSelection stc (pasteIndented before pasted)
    styledTextCtrlEndUndoAction stc

-- | The text on the clipboard, if there is any.
clipboardText :: IO (Maybe String)
clipboardText = do
  cb <- clipboardCreate
  opened <- clipboardOpen cb
  if not opened then pure Nothing else do
    obj <- textDataObjectCreate ""
    got <- clipboardGetData cb obj
    txt <- textDataObjectGetText obj
    clipboardClose cb
    textDataObjectDelete obj
    pure (if got then Just txt else Nothing)

-- | Comment out, or uncomment, the lines the selection touches (or the
-- cursor's line), as "Language.Editor.toggleComments" does, and select
-- them -- or, without a selection, keep the cursor where it was in its
-- line's text.
toggleCommentLines :: StyledTextCtrl a -> IO ()
toggleCommentLines stc = do
  from <- styledTextCtrlGetSelectionStart stc
  to <- styledTextCtrlGetSelectionEnd stc
  firstLine <- styledTextCtrlLineFromPosition stc from
  lastLine' <- styledTextCtrlLineFromPosition stc to
  lastLineStart <- styledTextCtrlPositionFromLine stc lastLine'
  -- a selection that ends at the start of a line doesn't take it in
  let lastLine = if to > from && to == lastLineStart && lastLine' > firstLine
                   then lastLine' - 1 else lastLine'
  linesStart <- styledTextCtrlPositionFromLine stc firstLine
  linesEnd <- styledTextCtrlGetLineEndPosition stc lastLine
  old <- styledTextCtrlGetTextRange stc linesStart linesEnd
  let new = intercalate "\n" (toggleComments (splitLines old))
      bytes = BS.length . Utf8.fromString
      grown = bytes new - bytes old
  styledTextCtrlBeginUndoAction stc
  styledTextCtrlSetTargetStart stc linesStart
  styledTextCtrlSetTargetEnd stc linesEnd
  _ <- styledTextCtrlReplaceTarget stc new
  styledTextCtrlEndUndoAction stc
  if from == to
    then styledTextCtrlGotoPos stc (max linesStart (from + grown))
    else styledTextCtrlSetSelection stc linesStart (linesStart + bytes new)
  where
    splitLines s = case break (== '\n') s of
      (l, []) -> [l]
      (l, _ : more) -> l : splitLines more

-- | Break the line, starting the new one indented as "Language.Editor"
-- says. Spaces around the break go: the ones before it would be left
-- trailing, and the ones after it would add to the indentation.
newLine :: StyledTextCtrl a -> IO ()
newLine stc = do
  styledTextCtrlBeginUndoAction stc
  styledTextCtrlReplaceSelection stc ""
  pos <- styledTextCtrlGetCurrentPos stc
  lineNum <- styledTextCtrlLineFromPosition stc pos
  lineStart <- styledTextCtrlPositionFromLine stc lineNum
  lineEnd <- styledTextCtrlGetLineEndPosition stc lineNum
  before <- styledTextCtrlGetTextRange stc lineStart pos
  after <- styledTextCtrlGetTextRange stc pos lineEnd
  let indent = if all (== ' ') before then length before else newLineIndent before
      -- (spaces are a byte each, so these are also byte counts)
      trailing = length (takeWhile (== ' ') (reverse before))
      leading = length (takeWhile (== ' ') after)
      from = if all (== ' ') before then pos else pos - trailing
      inserted = "\n" ++ replicate indent ' '
  styledTextCtrlSetTargetStart stc from
  styledTextCtrlSetTargetEnd stc (pos + leading)
  _ <- styledTextCtrlReplaceTarget stc inserted
  styledTextCtrlGotoPos stc (from + length inserted)
  styledTextCtrlEndUndoAction stc

-- | The editor action for a key, if it's an emacs key: a letter with the
-- Control key (only), however the platform reports it. On macOS,
-- Control-E arrives as the control character it types (ENQ, 5), with no
-- modifiers at all; so a control character is taken to be its letter
-- with Control. (Control-H, I and M arrive as the backspace, tab and
-- return keys, which is what they mean anyway.)
emacsCommand :: EventKey -> Maybe (StyledTextCtrl a -> IO ())
emacsCommand k
  | altDown (keyModifiers k) = Nothing
  | otherwise = case keyKey k of
      KeyChar c
        | ord c >= 1 && ord c <= 26 -> lookup (chr (ord c + ord 'a' - 1)) bindings
        | isControl (keyModifiers k) -> lookup (toLower c) bindings
      _ -> Nothing
  where
    bindings =
      [ ('a', run wxSTC_CMD_HOME)
      , ('e', run wxSTC_CMD_LINEEND)
      , ('f', run wxSTC_CMD_CHARRIGHT)
      , ('b', run wxSTC_CMD_CHARLEFT)
      , ('n', run wxSTC_CMD_LINEDOWN)
      , ('p', run wxSTC_CMD_LINEUP)
      , ('d', run wxSTC_CMD_CLEAR)
      , ('k', killLine)
      , ('o', openLine)
      , ('t', transpose)
      , ('v', run wxSTC_CMD_PAGEDOWN)
      , ('l', run sciVerticalCentreCaret)
      ]
    run cmd stc = styledTextCtrlCmdKeyExecute stc cmd
    -- SCI_VERTICALCENTRECARET, which wxHaskell has no name for
    sciVerticalCentreCaret = 2619

-- | Only the Control key (not Command, on macOS, which wx calls
-- "control" there).
isControl :: Modifiers -> Bool
isControl m = not (altDown m) && not (shiftDown m) && case os of
  "darwin" -> metaDown m && not (controlDown m)
  _ -> controlDown m && not (metaDown m)

-- | Delete to the linesEnd of the line, or, at the linesEnd of a line, join it with
-- the next (as emacs's C-k does, without keeping what it deletes).
killLine :: StyledTextCtrl a -> IO ()
killLine stc = do
  pos <- styledTextCtrlGetCurrentPos stc
  lineNum <- styledTextCtrlLineFromPosition stc pos
  linesEnd <- styledTextCtrlGetLineEndPosition stc lineNum
  styledTextCtrlCmdKeyExecute stc (if pos == linesEnd then wxSTC_CMD_CLEAR else wxSTC_CMD_DELLINERIGHT)

-- | Break the line here, leaving the cursor where it was.
openLine :: StyledTextCtrl a -> IO ()
openLine stc = do
  pos <- styledTextCtrlGetCurrentPos stc
  styledTextCtrlInsertText stc pos "\n"

-- | Swap the characters before and after the cursor, and move past them
-- (at the linesEnd of a line, swap the two before it).
transpose :: StyledTextCtrl a -> IO ()
transpose stc = do
  pos <- styledTextCtrlGetCurrentPos stc
  lineNum <- styledTextCtrlLineFromPosition stc pos
  lineStart <- styledTextCtrlPositionFromLine stc lineNum
  linesEnd <- styledTextCtrlGetLineEndPosition stc lineNum
  let at p = styledTextCtrlPositionAfter stc p
      before p = styledTextCtrlPositionBefore stc p
  when (linesEnd - lineStart >= 2) $ do
    -- the two characters to swap, as byte ranges (they can be several
    -- bytes each, in UTF-8)
    mid <- if pos >= linesEnd then before linesEnd else pure pos
    a <- before mid
    b <- at mid
    when (a >= lineStart && b <= linesEnd && a < mid && mid < b) $ do
      one <- styledTextCtrlGetTextRange stc a mid
      two <- styledTextCtrlGetTextRange stc mid b
      styledTextCtrlBeginUndoAction stc
      styledTextCtrlSetTargetStart stc a
      styledTextCtrlSetTargetEnd stc b
      _ <- styledTextCtrlReplaceTarget stc (two ++ one)
      styledTextCtrlEndUndoAction stc
      styledTextCtrlGotoPos stc b
