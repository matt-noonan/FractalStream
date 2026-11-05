-- | Typing mathematical symbols by their LaTeX-style names in a code
-- editor ("Language.Editor.Symbols"): typing @\\@ starts a name, and the
-- name turns into its symbol when it's finished.
--
-- * Letters go on with the name. While it's typed, it's boxed, and a
--   call tip under it shows what it would become (and what longer names
--   start with it).
--
-- * Anything else that's typed finishes it: @\\theta@ and a space is θ
--   and a space. Tab finishes it without typing anything, and Return
--   finishes it and breaks the line ("UI.EditorKeys"). Tab also
--   completes a name: @\\th@ is θ, as the only name that starts that
--   way, and @\\va@ goes on to @\\var@ ("Language.Editor.Symbols.
--   completeSymbolName"). A second @\\@
--   finishes it and starts another (@\\alpha\\beta@ is αβ), but a @\\@
--   right after a @\\@ is just the two backslashes (so @\\\\theta@ stays
--   as it is, in a string that's meant to have a backslash in it).
--
-- * A name that isn't one is left as it was typed, and so is any name
--   after Escape, moving the cursor out of it, or leaving the editor.
--
-- Turning a name into its symbol is its own undo step: undo gives back
-- the name.
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

import Language.Editor.Symbols (symbolNamed, isSymbolNameChar, symbolPreview, Completion(..), completeSymbolName)

-- | A code editor's name being typed, if there is one.
data SymbolEntry a = SymbolEntry
  { seCtrl :: StyledTextCtrl a
  , sePending :: IORef (Maybe Int)
    -- ^ where the name's backslash is (a byte position), while one is typed
  , seBoxed :: IORef (Maybe (Int, Int))
    -- ^ the range that's boxed
  , seTip :: IORef (Maybe (Int, String))
    -- ^ the call tip that's showing, and where
  }

-- | Get an editor ready for typing names.
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

-- | A character has been typed (and is in the text, before the cursor).
symbolCharAdded :: SymbolEntry a -> Char -> IO ()
symbolCharAdded se c = do
  let stc = seCtrl se
  pos <- styledTextCtrlGetCurrentPos stc
  -- (where the character went: the position Scintilla reports with it
  -- can't be relied on)
  let typedAt = pos - BS.length (Utf8.fromChar c)
  readIORef (sePending se) >>= \case
    Nothing -> when (c == '\\') (writeIORef (sePending se) (Just typedAt))
    Just start
      | isSymbolNameChar c -> pure ()
      -- a backslash right after the backslash: two backslashes
      | c == '\\', typedAt == start + 1 -> writeIORef (sePending se) Nothing
      | otherwise -> do
          convert se start typedAt
          when (c == '\\') $ do
            pos' <- styledTextCtrlGetCurrentPos stc
            writeIORef (sePending se) (Just (pos' - 1))

-- | The text has changed: @len@ bytes inserted or deleted at @pos@ (as
-- Scintilla's modification type @modType@ says). The name being typed
-- moves with the text before it, and ends if its backslash goes.
--
-- (This only changes what's remembered: nothing may be done to the
-- editor while it reports a change. 'symbolUpdateUI' catches up.)
symbolTextModified :: SymbolEntry a -> Int -> Int -> Int -> IO ()
symbolTextModified se pos modType len = readIORef (sePending se) >>= \case
  Nothing -> pure ()
  Just start
    | modType .&. sciModInsertText /= 0, pos <= start ->
        writeIORef (sePending se) (Just (start + len))
    | modType .&. sciModDeleteText /= 0, pos + len <= start ->
        writeIORef (sePending se) (Just (start - len))
    | modType .&. sciModDeleteText /= 0, pos <= start ->
        writeIORef (sePending se) Nothing
    | otherwise -> pure ()

-- | The editor has been updated (its text, or where the cursor is): show
-- the name being typed, boxed and with its preview, or stop typing it if
-- the cursor has left it.
symbolUpdateUI :: SymbolEntry a -> IO ()
symbolUpdateUI se = readIORef (sePending se) >>= \case
  Nothing -> clearShown se
  Just start -> do
    let stc = seCtrl se
    name <- nameAt stc start
    caret <- styledTextCtrlGetCurrentPos stc
    anchor <- styledTextCtrlGetAnchor stc
    case name of
      Just n | caret == anchor, caret > start, caret <= start + 1 + length n -> do
        box se start (start + 1 + length n)
        tip se start n
      _ -> do
        writeIORef (sePending se) Nothing
        clearShown se

-- | Turn the name being typed into its symbol (or leave it as it is,
-- if it isn't a name). Whether a name was being typed.
finishSymbol :: SymbolEntry a -> IO Bool
finishSymbol se = readIORef (sePending se) >>= \case
  Nothing -> pure False
  Just start -> do
    name <- nameAt (seCtrl se) start
    case name of
      Just n -> convert se start (start + 1 + length n)
      Nothing -> endName se
    pure True

-- | Complete the name being typed, if it can be ('completeSymbolName'):
-- turn it into its symbol, or go on with it. Whether a name was being
-- typed (if one was, it's still being typed unless it became a symbol).
completeSymbol :: SymbolEntry a -> IO Bool
completeSymbol se = readIORef (sePending se) >>= \case
  Nothing -> pure False
  Just at -> do
    let stc = seCtrl se
    nameAt stc at >>= \case
      Nothing -> endName se
      Just n -> do
        let end = at + 1 + length n
        case completeSymbolName n of
          Just (Finished sym) -> replaceName se at end sym
          Just (Longer n') -> do
            styledTextCtrlInsertText stc end (drop (length n) n')
            styledTextCtrlGotoPos stc (at + 1 + length n')
          Nothing -> pure ()
    pure True

-- | Stop typing a name, leaving it as it is. Whether a name was being
-- typed.
cancelSymbol :: SymbolEntry a -> IO Bool
cancelSymbol se = readIORef (sePending se) >>= \case
  Nothing -> pure False
  Just _ -> endName se >> pure True

-- | The name whose backslash is at @start@, if that's a backslash. (A
-- name is ASCII letters, so its length in characters is its length in
-- bytes.)
nameAt :: StyledTextCtrl a -> Int -> IO (Maybe String)
nameAt stc start = do
  docLen <- styledTextCtrlGetLength stc
  if start < 0 || start >= docLen then pure Nothing else do
    lineNum <- styledTextCtrlLineFromPosition stc start
    lineEnd <- styledTextCtrlGetLineEndPosition stc lineNum
    styledTextCtrlGetTextRange stc start lineEnd <&> \case
      '\\' : rest -> Just (takeWhile isSymbolNameChar rest)
      _ -> Nothing

-- | Replace the backslash at @start@ and the name after it, up to @end@,
-- with its symbol, if it's a name; and stop typing it either way.
convert :: SymbolEntry a -> Int -> Int -> IO ()
convert se start end = do
  let stc = seCtrl se
  endName se
  name <- styledTextCtrlGetTextRange stc start end
  case name of
    '\\' : n | Just sym <- symbolNamed n -> replaceName se start end sym
    _ -> pure ()

-- | Replace the text from @start@ to @end@ (a backslash and a name) with
-- a symbol, as an undo step of its own, and stop typing the name. The
-- cursor stays after it, if it was in it or after it.
replaceName :: SymbolEntry a -> Int -> Int -> String -> IO ()
replaceName se start end sym = do
  let stc = seCtrl se
  endName se
  caret <- styledTextCtrlGetCurrentPos stc
  let symLen = BS.length (Utf8.fromString sym)
  styledTextCtrlBeginUndoAction stc
  styledTextCtrlSetTargetStart stc start
  styledTextCtrlSetTargetEnd stc end
  _ <- styledTextCtrlReplaceTarget stc sym
  styledTextCtrlEndUndoAction stc
  when (caret >= start) $ styledTextCtrlGotoPos stc $
    if caret >= end then caret - (end - start) + symLen else start + symLen

-- | Stop typing a name, and stop showing it.
endName :: SymbolEntry a -> IO ()
endName se = do
  writeIORef (sePending se) Nothing
  clearShown se

-- | Box the range from @from@ to @to@ (only).
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

-- | Show the preview for name @n@, whose backslash is at @start@ (or no
-- call tip, if there's nothing to show yet).
tip :: SymbolEntry a -> Int -> String -> IO ()
tip se start n = do
  let stc = seCtrl se
  shown <- readIORef (seTip se)
  active <- styledTextCtrlCallTipActive stc
  case symbolPreview n of
    Nothing -> when (isJust shown) $ do
      styledTextCtrlCallTipCancel stc
      writeIORef (seTip se) Nothing
    Just (line, exact) -> when (shown /= Just (start, line) || not active) $ do
      styledTextCtrlCallTipShow stc start line
      -- (the highlight is in bytes of the tip)
      let bytes = BS.length . Utf8.fromString
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

-- | The indicator that boxes a name being typed (the one after
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
