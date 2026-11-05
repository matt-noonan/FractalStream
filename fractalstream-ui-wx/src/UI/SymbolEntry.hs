-- | Typing mathematical symbols by their LaTeX-style names in a code
-- editor ("Language.Editor.Symbols"): typing @\\@ starts an escape,
-- which turns into its symbol when it's finished.
--
-- * Letters go on with a name. While an escape is typed, it's boxed,
--   and a call tip under it shows what it would become (and what longer
--   names start with it).
--
-- * Anything else that's typed finishes a symbol's name: @\\theta@ and
--   a space is θ and a space. An accent's name waits for what it goes on
--   (the next character, after any spaces, or a name, or one character
--   in braces), so that @\\hat x@, @\\hat{x}@ and @\\hat\\theta@ are
--   x̂, x̂ and θ̂ ("Language.Editor.Symbols.escapeAt").
--
-- * Tab completes a name ("Language.Editor.Symbols.completeSymbolName")
--   without typing anything: @\\th@ is θ, as the only name that starts
--   that way, @\\va@ goes on to @\\var@, and @\\ha@ is @\\hat@,
--   waiting for what it goes on. Return finishes an escape (as at the
--   end of the text) and breaks the line ("UI.EditorKeys").
--
-- * A second @\\@ finishes a name and starts another (@\\alpha\\beta@
--   is αβ), but a @\\@ right after a @\\@ is just the two backslashes
--   (so @\\\\theta@ stays as it is, in a string that's meant to have a
--   backslash in it).
--
-- * A name that isn't one is left as it was typed, and so is any escape
--   after Escape, moving the cursor out of it, or leaving the editor.
--
-- Turning an escape into its symbol is its own undo step: undo gives
-- back what was typed.
--
-- The editor sends this its typed characters, changes, and updates
-- ("UI.CodeEditor"), and Tab, Return and Escape ("UI.EditorKeys").
module UI.SymbolEntry
  ( SymbolEntry
  , symbolEntry
  , symbolCharAdded
  , symbolTextModified
  , symbolUpdateUI
  , finishSymbol
  , completeSymbol
  , cancelSymbol
  ) where

import FractalStream.Prelude hiding (get)

import Graphics.UI.WX (rgb)
import Graphics.UI.WXCore.WxcClasses

import Data.Bits ((.&.))
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as Utf8

import Language.Editor.Symbols
  (Escape(..), escapeAt, escapePreview, typedName, Completion(..), completeSymbolName)

-- | A code editor's escape being typed, if there is one.
data SymbolEntry a = SymbolEntry
  { seCtrl :: StyledTextCtrl a
  , sePending :: IORef (Maybe (Int, Int))
    -- ^ where the escape is (byte positions, from its backslash up to
    -- the end of what's been typed), while one is typed
  , seBoxed :: IORef (Maybe (Int, Int))
    -- ^ the range that's boxed
  , seTip :: IORef (Maybe (Int, String))
    -- ^ the call tip that's showing, and where
  }

-- | Get an editor ready for typing escapes.
symbolEntry :: StyledTextCtrl a -> Bool -> IO (SymbolEntry a)
symbolEntry stc isLight = do
  styledTextCtrlIndicatorSetStyle stc nameIndicator sciIndicRoundBox
  styledTextCtrlIndicatorSetForeground stc nameIndicator
    (if isLight then rgb 0x30 0x70 (0xd0 :: Word8) else rgb 0x70 0xa8 (0xff :: Word8))
  void (styledTextCtrlSendMsg stc sciIndicSetAlpha nameIndicator 40)
  void (styledTextCtrlSendMsg stc sciIndicSetOutlineAlpha nameIndicator 140)
  void (styledTextCtrlSendMsg stc sciIndicSetUnder nameIndicator 1)
  if isLight
    then do
      styledTextCtrlCallTipSetBackground stc (rgb 0xf4 0xf4 (0xf4 :: Word8))
      styledTextCtrlCallTipSetForeground stc (rgb 0x60 0x60 (0x60 :: Word8))
      styledTextCtrlCallTipSetForegroundHighlight stc (rgb 0x00 0x40 (0xc0 :: Word8))
    else do
      styledTextCtrlCallTipSetBackground stc (rgb 0x2a 0x31 (0x38 :: Word8))
      styledTextCtrlCallTipSetForeground stc (rgb 0xa0 0xa0 (0xa0 :: Word8))
      styledTextCtrlCallTipSetForegroundHighlight stc (rgb 0xff 0xff (0xff :: Word8))
  SymbolEntry stc <$> newIORef Nothing <*> newIORef Nothing <*> newIORef Nothing

-- | A character has been typed (and is in the text, before the cursor;
-- if it went into the escape being typed, 'symbolTextModified' has
-- already taken it in).
symbolCharAdded :: SymbolEntry a -> Char -> IO ()
symbolCharAdded se c = readIORef (sePending se) >>= \case
  Just escape -> settle se escape
  Nothing -> when (c == '\\') $ do
    -- (where the character went: the position Scintilla reports with it
    -- can't be relied on)
    pos <- styledTextCtrlGetCurrentPos (seCtrl se)
    writeIORef (sePending se) (Just (pos - bytes [c], pos))

-- | Turn the escape being typed into its symbol, if it's finished; or
-- stop typing it, if it can't be one ('escapeAt'). What's after a
-- finished escape stays, and if that's a backslash, it starts another.
settle :: SymbolEntry a -> (Int, Int) -> IO ()
settle se (start, end) = do
  let stc = seCtrl se
  text <- styledTextCtrlGetTextRange stc start end
  case escapeAt False text of
    Incomplete -> pure ()
    Literal -> endEscape se
    NotAName -> do
      endEscape se
      -- (a backslash after a name that isn't one starts another)
      when (length text > 1 && last text == '\\') $
        writeIORef (sePending se) (Just (end - 1, end))
    Complete new n -> do
      let finishedEnd = start + bytes (take n text)
          rest = drop n text
      replace se start finishedEnd new
      when ("\\" `isPrefixOf` rest) $ do
        let restStart = start + bytes new
            escape = (restStart, restStart + bytes rest)
        writeIORef (sePending se) (Just escape)
        settle se escape

-- | The text has changed: @len@ bytes inserted or deleted at @pos@ (as
-- Scintilla's modification type @modType@ says). The escape being typed
-- moves with the text before it, takes in what's typed into it or at its
-- end, and ends if its backslash goes.
--
-- (This only changes what's remembered: nothing may be done to the
-- editor while it reports a change. 'symbolUpdateUI' catches up.)
symbolTextModified :: SymbolEntry a -> Int -> Int -> Int -> IO ()
symbolTextModified se pos modType len = readIORef (sePending se) >>= \case
  Nothing -> pure ()
  Just (start, end)
    | modType .&. sciModInsertText /= 0 -> if
        | pos <= start -> set' (Just (start + len, end + len))
        | pos <= end -> set' (Just (start, end + len))
        | otherwise -> pure ()
    | modType .&. sciModDeleteText /= 0 -> if
        | pos + len <= start -> set' (Just (start - len, end - len))
        | pos <= start -> set' Nothing
        | pos >= end -> pure ()
        | otherwise -> set' (Just (start, end - (min end (pos + len) - pos)))
    | otherwise -> pure ()
  where set' = writeIORef (sePending se)

-- | The editor has been updated (its text, or where the cursor is): show
-- the escape being typed, boxed and with its preview, or stop typing it
-- if the cursor has left it.
symbolUpdateUI :: SymbolEntry a -> IO ()
symbolUpdateUI se = readIORef (sePending se) >>= \case
  Nothing -> clearShown se
  Just (start, end) -> do
    let stc = seCtrl se
    docLen <- styledTextCtrlGetLength stc
    caret <- styledTextCtrlGetCurrentPos stc
    anchor <- styledTextCtrlGetAnchor stc
    text <- if end <= docLen then styledTextCtrlGetTextRange stc start end else pure ""
    if "\\" `isPrefixOf` text && caret == anchor && caret > start && caret <= end
      then do
        box se start end
        tip se start text
      else do
        writeIORef (sePending se) Nothing
        clearShown se

-- | Finish the escape being typed, as if the text ended there (so a name
-- becomes its symbol, but an accent with nothing to go on stays as it
-- is). Whether an escape was being typed.
finishSymbol :: SymbolEntry a -> IO Bool
finishSymbol se = readIORef (sePending se) >>= \case
  Nothing -> pure False
  Just (start, end) -> do
    text <- styledTextCtrlGetTextRange (seCtrl se) start end
    case escapeAt True text of
      Complete new n -> replace se start (start + bytes (take n text)) new
      _ -> endEscape se
    pure True

-- | Complete the name being typed at the end of the escape, if it can be
-- ('completeSymbolName'): turn it into its symbol, or go on with it. An
-- accent waiting for what it goes on stays as it is. Whether an escape
-- was being typed.
completeSymbol :: SymbolEntry a -> IO Bool
completeSymbol se = readIORef (sePending se) >>= \case
  Nothing -> pure False
  Just (start, end) -> do
    let stc = seCtrl se
    text <- styledTextCtrlGetTextRange stc start end
    forM_ (typedName text) $ \name -> do
      let nameStart = end - bytes ('\\' : name)
          goOn more = do
            -- (typed at the end, so the escape takes it in)
            styledTextCtrlInsertText stc end more
            styledTextCtrlGotoPos stc (end + bytes more)
      case completeSymbolName name of
        Nothing -> pure ()
        Just (Longer longer) -> goOn (drop (length name) longer)
        -- an accent: its whole name, and a space (to go on with what
        -- it goes on)
        Just (Finished new) | "\\" `isPrefixOf` new -> goOn (drop (1 + length name) new)
        Just (Finished new)
          | nameStart == start -> replace se start end new
          | otherwise -> do
              -- a name inside the escape (an accent's, \hat\theta):
              -- the escape goes on, with the symbol in it
              styledTextCtrlBeginUndoAction stc
              styledTextCtrlSetTargetStart stc nameStart
              styledTextCtrlSetTargetEnd stc end
              _ <- styledTextCtrlReplaceTarget stc new
              styledTextCtrlEndUndoAction stc
              styledTextCtrlGotoPos stc (nameStart + bytes new)
              readIORef (sePending se) >>= mapM_ (settle se)
    pure True

-- | Stop typing an escape, leaving it as it is. Whether an escape was
-- being typed.
cancelSymbol :: SymbolEntry a -> IO Bool
cancelSymbol se = readIORef (sePending se) >>= \case
  Nothing -> pure False
  Just _ -> endEscape se >> pure True

-- | Replace the text from @start@ to @end@ (a finished escape) with what
-- it becomes, as an undo step of its own, and stop typing the escape.
-- The cursor stays after it, if it was in it or after it.
replace :: SymbolEntry a -> Int -> Int -> String -> IO ()
replace se start end new = do
  let stc = seCtrl se
  endEscape se
  caret <- styledTextCtrlGetCurrentPos stc
  let newLen = bytes new
  styledTextCtrlBeginUndoAction stc
  styledTextCtrlSetTargetStart stc start
  styledTextCtrlSetTargetEnd stc end
  _ <- styledTextCtrlReplaceTarget stc new
  styledTextCtrlEndUndoAction stc
  when (caret >= start) $ styledTextCtrlGotoPos stc $
    if caret >= end then caret - (end - start) + newLen else start + newLen

-- | Stop typing an escape, and stop showing it.
endEscape :: SymbolEntry a -> IO ()
endEscape se = do
  writeIORef (sePending se) Nothing
  clearShown se

-- | How many bytes some text is, in UTF-8 (as Scintilla's positions
-- count).
bytes :: String -> Int
bytes = BS.length . Utf8.fromString

box :: SymbolEntry a -> Int -> Int -> IO ()
box se from to = do
  boxed <- readIORef (seBoxed se)
  when (boxed /= Just (from, to)) $ do
    let stc = seCtrl se
    unbox stc
    void (styledTextCtrlSendMsg stc sciSetIndicatorCurrent nameIndicator 0)
    void (styledTextCtrlSendMsg stc sciIndicatorFillRange from (to - from))
    writeIORef (seBoxed se) (Just (from, to))

unbox :: StyledTextCtrl a -> IO ()
unbox stc = do
  docLen <- styledTextCtrlGetLength stc
  void (styledTextCtrlSendMsg stc sciSetIndicatorCurrent nameIndicator 0)
  void (styledTextCtrlSendMsg stc sciIndicatorClearRange 0 docLen)

-- | Show the preview for the escape typed so far, @text@, whose
-- backslash is at @start@ (or no call tip, if there's nothing to show
-- yet).
tip :: SymbolEntry a -> Int -> String -> IO ()
tip se start text = do
  let stc = seCtrl se
  shown <- readIORef (seTip se)
  active <- styledTextCtrlCallTipActive stc
  case escapePreview text of
    Nothing -> when (isJust shown) $ do
      styledTextCtrlCallTipCancel stc
      writeIORef (seTip se) Nothing
    Just (line, exact) -> when (shown /= Just (start, line) || not active) $ do
      styledTextCtrlCallTipShow stc start line
      -- (the highlight is in bytes of the tip)
      case exact of
        Just (from, to) -> styledTextCtrlCallTipSetHighlight stc
          (bytes (take from line)) (bytes (take to line))
        Nothing -> styledTextCtrlCallTipSetHighlight stc 0 0
      writeIORef (seTip se) (Just (start, line))

clearShown :: SymbolEntry a -> IO ()
clearShown se = do
  let stc = seCtrl se
  readIORef (seBoxed se) >>= \case
    Nothing -> pure ()
    Just _ -> unbox stc >> writeIORef (seBoxed se) Nothing
  readIORef (seTip se) >>= \case
    Nothing -> pure ()
    Just _ -> styledTextCtrlCallTipCancel stc >> writeIORef (seTip se) Nothing

-- | The indicator that boxes an escape being typed (the one after
-- "UI.CodeEditor"'s error underline).
nameIndicator :: Int
nameIndicator = 9

-- Scintilla's numbers for these (from Scintilla.h)
sciSetIndicatorCurrent, sciIndicatorFillRange, sciIndicatorClearRange :: Int
sciSetIndicatorCurrent = 2500
sciIndicatorFillRange = 2504
sciIndicatorClearRange = 2505
sciIndicSetUnder, sciIndicSetAlpha, sciIndicSetOutlineAlpha, sciIndicRoundBox :: Int
sciIndicSetUnder = 2510
sciIndicSetAlpha = 2523
sciIndicSetOutlineAlpha = 2558
sciIndicRoundBox = 7  -- INDIC_ROUNDBOX
sciModInsertText, sciModDeleteText :: Int
sciModInsertText = 0x1
sciModDeleteText = 0x2
