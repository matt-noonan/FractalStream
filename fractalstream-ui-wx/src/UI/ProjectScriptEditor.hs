-- | A single editor window for a whole unified-script project. Reuses
-- 'UI.Widgets.scriptWidget' (the same live syntax-coloring and inline
-- error feedback a single viewer's own code editor gets) for the
-- editing surface, backed by a reactive 'Mapped' running
-- 'Actor.Ensemble.checkScript' (a pure, allocation-free parse +
-- cross-stanza-reference check) rather than
-- 'Actor.Ensemble.buildEnsembleFromScript' itself -- which can't be a
-- 'Mapped's pure mapper (allocating fresh 'Variable's is 'IO'), and
-- which this editor deliberately doesn't want recomputed on every
-- keystroke anyway (tearing down and rebuilding every viewer window
-- is disruptive, so it's an explicit "Rebuild" button, not something
-- reactive).
module UI.ProjectScriptEditor
  ( editProjectScript
  ) where

import FractalStream.Prelude hiding (get)

import Actor.UI (UI)
import Actor.Ensemble
import Actor.Layout (CodeString(..))

import Backend (Backend)
import UI.CodeEditor (stanzaHeaderStyle, editorOffsetMap)
import UI.Widgets (scriptWidget, wxWatchDynamic)
import Data.DynamicValue

import Graphics.UI.WX hiding (pt, when, grey, Color, Object, Layout, Vertical, Horizontal)
import Graphics.UI.WXCore.WxcClassesMZ
import Graphics.UI.WXCore.Frame (frameDefaultStyle, windowChildren)

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
-- FORMERLY A CRITICAL BUG, believed fixed (see the comment on the
-- `windowDestroy` call below for the root cause and fix; confirmed
-- against the deterministic repro described there, though not yet
-- stress-tested beyond it -- e.g. with several viewers, or many
-- rebuild cycles in a row). A rebuild used to be able to hang the
-- whole app (beachball, no redraw at all -- the main/GUI thread
-- itself, not just a background worker), reliably *deterministically*
-- on the second successful Rebuild of a session, not probabilistically.
--
-- Two other real bugs were found and fixed along the way while
-- chasing this, neither of which turned out to be the cause on its
-- own (ruled out under the same deterministic repro, after fixing
-- each):
--   1. 'cancelTile' propagating 'AsyncCancelled' back into whatever
--      thread called 'link' on the tile's worker -- checked and ruled
--      out structurally (the installed @async@ package's source:
--      @link = linkOnly (not . isCancel)@) without needing a code
--      change.
--   2. This editor used to run its own periodic change-detection
--      'wxTimer' (since replaced by reusing 'UI.Widgets.scriptWidget',
--      see the module header), owned by `p` (this editor's panel)
--      rather than a frame -- 'wxTimer's cleanup hooks into the
--      owner's "closing" event, and panels never receive
--      @wxEVT_CLOSE_WINDOW@ regardless of what happens to their
--      parent, so this genuinely leaked a timer polling an already-
--      destroyed 'StyledTextCtrl' on every close (the identical bug
--      in 'UI.Widgets.scriptWidget' itself was fixed the same way,
--      and still applies here since this editor now shares that
--      code). Worth having fixed regardless, but on its own this was
--      only ever a symptom of the bigger problem below, not the
--      problem itself.
--
-- (Also tried, before realizing the hang was deterministic rather
-- than probabilistic, and reverted as unneeded once it didn't help:
-- deferring the whole teardown-and-rebuild to a later event-loop tick
-- via a one-shot 'timer' owned by `projectWindow`, on the theory it
-- might be GUI reentrancy from closing `f` inside one of its own
-- widget's event handlers. See git history on this file if that
-- pattern is ever wanted again for something else.)
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

      -- 'checkScript' is pure (parse + cross-stanza-reference check,
      -- no allocation), so -- unlike 'buildEnsembleFromScript' -- it
      -- can drive a reactive 'Mapped', giving this editor the same
      -- live syntax-coloring and inline error squiggles a single
      -- viewer's own code editor gets from 'scriptWidget', for free.
      srcVar <- newVariable (CodeString srcText)
      scriptCheck <- newMapped (pure (\(CodeString s) -> checkScript s)) srcVar
      (ce, liveErrorText) <- scriptWidget p scriptCheck

      -- Stanza-header styling has no counterpart in a single viewer's
      -- own code (which never has `@`-lines), so 'scriptWidget' can't
      -- know to do it -- kept as this editor's own small addition,
      -- piggybacking on `scriptCheck`'s source (which 'scriptWidget'
      -- itself already keeps in sync with the widget's text) rather
      -- than a second polling timer of this editor's own.
      restyleStanzaHeaders ce srcText
      wxWatchDynamic f (source scriptCheck) $ \(CodeString newText) ->
        restyleStanzaHeaders ce newText

      -- Separate from `liveErrorText` above: that one reflects the
      -- *current* text's live check, continuously; this one reflects
      -- the *last explicit Rebuild attempt*, and is the only place a
      -- deeper error 'checkScript' doesn't catch (e.g. a config
      -- field's default not matching its declared type -- only
      -- found by 'buildWithValidTargets', which interleaves that
      -- check with allocation) ever gets reported.
      rebuildErrorText <- staticText p [ text := "", visible := False ]

      rebuildBtn <- button p [ text := "Rebuild" ]
      cancelBtn  <- button p [ text := "Cancel"
                             , on command := void (windowClose f True) ]

      set rebuildBtn [ on command := do
          newText <- styledTextCtrlGetText ce
          buildEnsembleFromScript newText >>= \case
            Left err -> do
              set rebuildErrorText [ text := formatScriptError newText err, visible := True ]
              windowReLayout f
            Right newEns -> do
              -- `windowDestroy`, not `windowClose ... True`: every
              -- viewer frame ('UI.ProjectViewer.makeWxComplexViewer')
              -- installs its own `windowOnClose` handler that just
              -- hides the window (meant to make a user's click on its
              -- native close button a reversible hide, not a
              -- destroy). `Close(force)` still always runs that
              -- installed handler and only controls whether its veto
              -- is honored -- force doesn't bypass the handler or
              -- destroy the window itself, so `windowClose` never
              -- actually closed a single viewer frame here, no matter
              -- the force flag: every rebuild leaked a whole, fully
              -- alive previous generation of viewer windows.
              -- `windowDestroy` calls wxWidgets' `Destroy()` directly,
              -- bypassing that installed handler entirely -- which is
              -- what a deliberate, programmatic teardown like this
              -- one actually wants, unlike a user's own accidental
              -- click. `f` is itself a child of `projectWindow`, so
              -- it's already included in this sweep.
              windowChildren projectWindow >>= mapM_ windowDestroy
              setValue' currentEnsemble newEns
              runEnsemble backend ui newEns
        ]

      set f [ layout := fill $ container p $ fill $ margin 5 $ column 5
                [ fill $ widget ce
                , hstretch $ expand $ widget liveErrorText
                , hstretch $ expand $ widget rebuildErrorText
                , hstretch $ expand $ row 5 [ widget rebuildBtn, widget cancelBtn ]
                ]
            , clientSize := sz 700 500
            ]
      void (windowShow f)

-- | Apply 'stanzaHeaderStyle' to every line that starts with @\@@ --
-- a unified script's @\@setup@\/@\@config@\/@\@viewer@\/@\@tool@ stanza
-- headers are always flush-left (the grammar requires it), so checking
-- column 0 is enough; no need to parse.
--
-- Deliberately does *not* reset the whole document to style 0 first
-- (as an earlier version did): this runs on every edit, on its own
-- timer, independently of -- and racing against -- 'UI.Widgets.
-- scriptWidget's own comment\/error-span coloring of the same
-- 'StyledTextCtrl'. A blanket reset-then-restyle would win that race
-- essentially every time (it fires far more often, on any edit at
-- all, not just when the error status actually flips) and wipe out
-- whatever 'scriptWidget' had just painted, leaving comments and
-- error spans visually invisible even though the error *text* (driven
-- separately) still showed up fine. Instead this only ever touches a
-- line that is a header now, or was one the last time this ran
-- (checked via the line's own current style, since 'stanzaHeaderStyle'
-- is never used for anything else) -- every other line, including
-- anything 'scriptWidget' has styled, is left completely alone.
restyleStanzaHeaders :: StyledTextCtrl a -> String -> IO ()
restyleStanzaHeaders stc txt = do
  let totalBytes = BS.length (Utf8.fromString txt)
      byteOffsets = editorOffsetMap txt
      byteOffsetAt i = Map.findWithDefault totalBytes i byteOffsets
      ls = lines txt
      lineStarts = scanl (\off l -> off + length l + 1) 0 ls

  forM_ (zip ls lineStarts) $ \(l, charStart) -> do
    let byteStart = byteOffsetAt charStart
        byteEnd   = byteOffsetAt (charStart + length l)
        isHeader  = "@" `isPrefixOf` l
    when (byteEnd > byteStart) $ do
      currentStyle <- styledTextCtrlGetStyleAt stc byteStart
      case (isHeader, currentStyle == stanzaHeaderStyle) of
        (True, False) -> do
          styledTextCtrlStartStyling stc byteStart 0
          styledTextCtrlSetStyling stc (byteEnd - byteStart) stanzaHeaderStyle
        (False, True) -> do
          styledTextCtrlStartStyling stc byteStart 0
          styledTextCtrlSetStyling stc (byteEnd - byteStart) 0
        _ -> pure ()
