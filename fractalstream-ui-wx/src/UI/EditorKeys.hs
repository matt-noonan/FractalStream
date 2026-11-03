-- | Keys in a code editor that do more than Scintilla does by itself:
--
-- * Return starts the new line indented as the language expects: as far
--   as the line it breaks, and a level further after a line ending in
--   @:@ ("Language.Editor"). (Backspace at the start of a line's code
--   takes off a level, which Scintilla does itself, given the indent
--   size.)
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
import Language.Editor (newLineIndent)

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
  windowOnKeyDown stc $ \k -> case keyKey k of
    KeyReturn | isPlain (keyModifiers k) -> newLine stc
    _ -> propagateEvent
  set stc [ on keyboard := \k -> do
    emacs <- emacsKeys
    case emacsCommand k of
      Just action | emacs -> action stc
      _ -> propagateEvent ]
  where
    isPlain m = not (altDown m || shiftDown m || controlDown m || metaDown m)

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

-- | Delete to the end of the line, or, at the end of a line, join it with
-- the next (as emacs's C-k does, without keeping what it deletes).
killLine :: StyledTextCtrl a -> IO ()
killLine stc = do
  pos <- styledTextCtrlGetCurrentPos stc
  lineNum <- styledTextCtrlLineFromPosition stc pos
  end <- styledTextCtrlGetLineEndPosition stc lineNum
  styledTextCtrlCmdKeyExecute stc (if pos == end then wxSTC_CMD_CLEAR else wxSTC_CMD_DELLINERIGHT)

-- | Break the line here, leaving the cursor where it was.
openLine :: StyledTextCtrl a -> IO ()
openLine stc = do
  pos <- styledTextCtrlGetCurrentPos stc
  styledTextCtrlInsertText stc pos "\n"

-- | Swap the characters before and after the cursor, and move past them
-- (at the end of a line, swap the two before it).
transpose :: StyledTextCtrl a -> IO ()
transpose stc = do
  pos <- styledTextCtrlGetCurrentPos stc
  lineNum <- styledTextCtrlLineFromPosition stc pos
  lineStart <- styledTextCtrlPositionFromLine stc lineNum
  end <- styledTextCtrlGetLineEndPosition stc lineNum
  let at p = styledTextCtrlPositionAfter stc p
      before p = styledTextCtrlPositionBefore stc p
  when (end - lineStart >= 2) $ do
    -- the two characters to swap, as byte ranges (they can be several
    -- bytes each, in UTF-8)
    mid <- if pos >= end then before end else pure pos
    a <- before mid
    b <- at mid
    when (a >= lineStart && b <= end && a < mid && mid < b) $ do
      one <- styledTextCtrlGetTextRange stc a mid
      two <- styledTextCtrlGetTextRange stc mid b
      styledTextCtrlBeginUndoAction stc
      styledTextCtrlSetTargetStart stc a
      styledTextCtrlSetTargetEnd stc b
      _ <- styledTextCtrlReplaceTarget stc (two ++ one)
      styledTextCtrlEndUndoAction stc
      styledTextCtrlGotoPos stc b
