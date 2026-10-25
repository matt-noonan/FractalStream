-- | A single editor window for a whole unified-script project, modeled
-- on 'UI.Widgets.scriptWidget's "edit a single viewer's code" pattern --
-- except a whole project's 'Actor.Ensemble.Ensemble' can't be rebuilt by
-- a 'Mapped's pure mapper (allocating fresh 'Variable's is an 'IO'
-- action), so this editor recompiles explicitly, on a button press,
-- rather than reactively as the text changes.
module UI.ProjectScriptEditor
  ( editProjectScript
  ) where

import FractalStream.Prelude hiding (get)

import Actor.UI (UI)
import Actor.Ensemble

import Backend (Backend)
import UI.CodeEditor (codeEditorWithText, updateFoldLevels, stanzaHeaderStyle, editorOffsetMap)
import UI.Widgets (wxTimer)
import Data.DynamicValue

import Graphics.UI.WX hiding (pt, when, grey, Color, Object, Layout, Vertical, Horizontal)
import Graphics.UI.WXCore.WxcClassesMZ
import Graphics.UI.WXCore.Frame (frameDefaultStyle, windowChildren)

import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as Utf8
import qualified Data.Map as Map

-- | Open the whole-project script editor, or explain why there isn't
-- one to open. The project's own session keeps the live 'Ensemble' in
-- @currentEnsemble@ (not a fixed value: a successful rebuild here
-- replaces it) and @ui@ is built lazily, referring to this very
-- function, so a window opened after a rebuild can itself trigger
-- another one.
--
-- KNOWN CRITICAL BUG, not yet fixed (observed ~25% of the time): a
-- successful rebuild can hang the whole app. Reproduce with: open the
-- editor, introduce a syntax error, Rebuild (fails, shows the error --
-- this step alone never hangs), fix the error, Rebuild again (succeeds
-- -- sometimes hangs here). Likely trigger: this is the only path that
-- closes *every* viewer window at once, cancelling every viewer's
-- in-flight render tile ('UI.Tile.cancelTile') simultaneously, where
-- the pre-existing single-viewer "Edit viewer script..." flow only
-- ever cancels one viewer's tile at a time -- so a latent race in that
-- rendering/cancellation machinery (not written for this feature) is
-- the leading suspect, not anything above.
--
-- Already ruled out, with evidence, so don't re-try these:
--   1. GUI reentrancy from closing `f` (this editor's own window) from
--      inside one of its own widget's event handlers. Tried explicitly
--      deferring the whole teardown-and-rebuild to a later event-loop
--      tick via a one-shot 'timer' owned by `projectWindow`; the hang
--      persisted, so this was both wrong and unneeded complexity (now
--      reverted -- see git history on this file if the pattern is
--      ever wanted again for something else).
--   2. 'cancelTile' propagating 'AsyncCancelled' back into whatever
--      thread called 'link' on the tile's worker. Checked the
--      installed @async@ package's source directly: @link = linkOnly
--      (not . isCancel)@, i.e. cancellation is already excluded from
--      what 'link' re-throws. Not the cause.
--
-- Next step when picking this back up: get a thread dump at the
-- moment of the hang (e.g. `lldb -p <pid>` then `thread backtrace
-- all` on macOS) rather than guessing at more structural changes --
-- that's what finally pins down which thread is actually stuck.
editProjectScript :: Backend -> Window a -> Variable Ensemble -> UI -> IO ()
editProjectScript backend projectWindow currentEnsemble ui = do
  prj <- getDynamic currentEnsemble
  getDynamic (ensembleSourceScript prj) >>= \case
    Nothing -> do
      d <- dialog projectWindow [ text := "Can't edit this project as a script" ]
      p <- panel d []
      msg <- staticText p
        [ text := "This project uses the classic format and doesn't have\n\
                   \a unified script to edit yet."
        ]
      ok <- button p [ text := "OK" ]
      set d [ layout := margin 15 $ container p $ column 10
                [ widget msg, hfloatRight (widget ok) ] ]
      _ <- showModal d (\done -> set ok [ on command := done Nothing ])
      pure ()

    Just srcText -> do
      f <- frameEx frameDefaultStyle
             [ text := "Edit project script", visible := False ] projectWindow
      p <- panel f []
      ce <- codeEditorWithText p srcText
      restyleStanzaHeaders ce srcText
      errorText <- staticText p [ text := "", visible := False ]

      -- Check for edits periodically (matching 'UI.Widgets.scriptWidget's
      -- own established pattern for this), refreshing both the fold
      -- margin and the stanza-header styling either can go stale.
      lastText <- newIORef srcText
      void $ wxTimer p [ interval := 200, enabled := True
                       , on command := do
                           newText <- styledTextCtrlGetText ce
                           oldText <- readIORef lastText
                           when (newText /= oldText) $ do
                             writeIORef lastText newText
                             updateFoldLevels ce newText
                             restyleStanzaHeaders ce newText
                       ]

      rebuildBtn <- button p [ text := "Rebuild" ]
      cancelBtn  <- button p [ text := "Cancel"
                             , on command := void (windowClose f True) ]

      set rebuildBtn [ on command := do
          newText <- styledTextCtrlGetText ce
          buildEnsembleFromScript newText >>= \case
            Left err -> do
              set errorText [ text := formatScriptError newText err, visible := True ]
              windowReLayout f
            Right newEns -> do
              -- `f` is itself a child of `projectWindow`, so it's
              -- already included in this sweep.
              windowChildren projectWindow >>= mapM_ (`windowClose` True)
              setValue' currentEnsemble newEns
              runEnsemble backend ui newEns
        ]

      set f [ layout := fill $ container p $ fill $ margin 5 $ column 5
                [ fill $ widget ce
                , hstretch $ expand $ widget errorText
                , hstretch $ expand $ row 5 [ widget rebuildBtn, widget cancelBtn ]
                ]
            , clientSize := sz 700 500
            ]
      void (windowShow f)

-- | Apply 'stanzaHeaderStyle' to every line that starts with @\@@ --
-- a unified script's @\@setup@\/@\@config@\/@\@viewer@\/@\@tool@ stanza
-- headers are always flush-left (the grammar requires it), so checking
-- column 0 is enough; no need to parse. Resets the whole document to
-- the default style first, the same way 'UI.Widgets.scriptWidget's own
-- comment-coloring does, so a line that's lost its leading @\@@ since
-- the last pass doesn't stay styled.
restyleStanzaHeaders :: StyledTextCtrl a -> String -> IO ()
restyleStanzaHeaders stc txt = do
  let totalBytes = BS.length (Utf8.fromString txt)
  styledTextCtrlStartStyling stc 0 0
  styledTextCtrlSetStyling stc totalBytes 0

  let byteOffsets = editorOffsetMap txt
      byteOffsetAt i = Map.findWithDefault totalBytes i byteOffsets
      ls = lines txt
      lineStarts = scanl (\off l -> off + length l + 1) 0 ls

  forM_ (zip ls lineStarts) $ \(l, charStart) ->
    when ("@" `isPrefixOf` l) $ do
      let byteStart = byteOffsetAt charStart
          byteEnd   = byteOffsetAt (charStart + length l)
      styledTextCtrlStartStyling stc byteStart 0
      styledTextCtrlSetStyling stc (byteEnd - byteStart) stanzaHeaderStyle
