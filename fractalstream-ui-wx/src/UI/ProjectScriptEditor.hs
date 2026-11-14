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
  , ScriptEditorHooks(..)
  , replaceProject
  , rebuildColorizers
  ) where

import Data.IORef
import Data.Unique (Unique, newUnique)
import FractalStream.Prelude hiding (get)

import Actor.UI (UI)
import Actor.Ensemble
import Actor.Viewer.Complex (ComplexViewer(..))
import Actor.Layout (CodeString(..))

import Backend (Backend)
import UI.Loading (whileLoading)
import Actor.Metadata (ProjectMetadata(..))
import UI.Widgets (scriptWidget, scriptEditMenu, onOwnDestroy)
import UI.WxProperties (WxLayout, rememberLayout, releaseViewerFrame, releaseConfigFrame, discardSpareFrames)
import Data.DynamicValue

import Graphics.UI.WX hiding (pt, when, grey, Color, Object, Layout, Vertical, Horizontal)
import Graphics.UI.WXCore.WxcClassesMZ
import Graphics.UI.WXCore.Frame (frameDefaultStyle, windowChildren)

import qualified Data.Map as Map

-- | How the script editor reaches the rest of its project's session.
data ScriptEditorHooks = ScriptEditorHooks
  { sehAfterClosingProject :: IO ()
    -- ^ Run if closing this editor closes the whole project (see below).
  , sehDraft :: IORef (Maybe (Unique, IO String))
    -- ^ Where the open editor (if any) offers its text, for saving.
  , sehEditor :: IORef (Maybe (Window ()))
    -- ^ The open editor's window, if any: rebuilding the project for
    -- some other reason (a new colorizer) leaves it open.
  , sehKeep :: IO [Window ()]
    -- ^ Other windows of the project that outlast its rebuilds (its
    -- autocolor editor): they aren't built from its script.
  , sehMarkUnsaved :: IO ()
  , sehAddMenuBar :: forall a. Frame a -> [Menu ()] -> IO ()
    -- ^ Give the editor its project's menu bar (on macOS, where Command+S
    -- then saves this project rather than whichever one's menu bar was
    -- showing) or just its keyboard shortcuts (elsewhere), with the
    -- given menus of its own (an Edit menu) in either case.
  }

-- | Open the whole-project script editor, or explain why there isn't
-- one to open. The project's own session keeps the live 'Ensemble' in
-- @currentEnsemble@ (not a fixed value: a successful rebuild here
-- replaces it) and @ui@ is built lazily, referring to this very
-- function, so a window opened after a rebuild can itself trigger
-- another one.
--
-- @hooks@ connect the editor to the rest of its project's session (see
-- 'ScriptEditorHooks').
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
editProjectScript :: Backend -> Window a -> ScriptEditorHooks -> WxLayout -> Variable Ensemble -> UI -> IO ()
editProjectScript backend projectWindow ScriptEditorHooks{..} wxLayout currentEnsemble ui = do
  prj <- getDynamic currentEnsemble
  srcText <- getDynamic (ensembleSourceScript prj)

  f <- frameEx frameDefaultStyle
         [ text := "Edit project script", visible := False ] projectWindow
  p <- panel f []

  -- A project whose script has never built (one opened with an error
  -- in its script) has no windows but this editor, so closing the
  -- editor closes the project, rather than leave it open with nothing
  -- to show for it. The project is closed just after the editor, by a
  -- one-shot timer, rather than from inside the editor's own closing.
  -- (A rebuild destroys the editor without "closing" it, so this never
  -- fires for a project that's just been successfully rebuilt.)
  set f [ on closing := do
            built <- ensembleBuilt <$> getDynamic currentEnsemble
            unless built $ do
              closer <- timer projectWindow [ interval := 1 ]
              set closer [ on command := do
                             set closer [ enabled := False ]
                             void (windowClose projectWindow True)
                             sehAfterClosingProject ]

            propagateEvent ]


  -- 'checkScript' is pure (parse + cross-stanza-reference check,
  -- no allocation), so -- unlike 'buildEnsembleFromScript' -- it
  -- can drive a reactive 'Mapped', giving this editor the same
  -- live syntax-coloring and inline error squiggles a single
  -- viewer's own code editor gets from 'scriptWidget', for free.
  -- (Checked with the colorizers chosen when the editor was opened.)
  choices0 <- getDynamic (ensembleColorizerChoices prj)
  srcVar <- newVariable (CodeString srcText)
  scriptCheck <- newMapped (pure (\(CodeString s) -> checkScriptWithColorizers choices0 s)) srcVar
  (ce, liveErrorText) <- scriptWidget p scriptCheck

  -- While this editor is open, what's in it is the project's script, as
  -- far as saving goes, whether or not it's been rebuilt yet (saving
  -- and running are separate, as in any editor); so editing it is an
  -- unsaved change, and the project's shortcuts (Ctrl+S, or Command+S,
  -- to save, ...) work from here.
  token <- newUnique
  writeIORef sehDraft (Just (token, styledTextCtrlGetText ce))
  writeIORef sehEditor (Just (objectCast f))
  onOwnDestroy f $ do
    -- (Unless another editor for this project has taken over since.)
    modifyIORef' sehDraft $ \case
      Just (t, _) | t == token -> Nothing
      other -> other
    modifyIORef' sehEditor $ \case
      Just w | w == objectCast f -> Nothing
      other -> other
  void (watchDynamic srcVar (const sehMarkUnsaved))
  editMenu <- scriptEditMenu ce
  sehAddMenuBar f [editMenu]

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
      choices <- getDynamic . ensembleColorizerChoices =<< getDynamic currentEnsemble
      buildEnsembleWithColorizers choices newText >>= \case
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
          --
          -- Remember how every window was left first, so that the
          -- rebuilt ones come back the same way rather than at their
          -- defaults; and carry over everything about the project that
          -- isn't in its script (scenarios, metadata, frontends' own
          -- keys), since a script alone has none of it.
          keep <- sehKeep
          void (replaceProject backend projectWindow wxLayout currentEnsemble ui keep newEns)
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

-- | Replace a project's windows (all but the given ones) with those of a
-- newly built 'Ensemble': remembering how each window was left first, so
-- that the new ones come back the same way rather than at their
-- defaults, and carrying over everything about the project that isn't
-- in its script (scenarios, metadata, values, frontends' own keys). See
-- the Rebuild button in 'editProjectScript' for why this is
-- 'windowDestroy' rather than 'windowClose'.
--
-- A viewer the new script still has (by its title), and the
-- configuration window, keep their windows: those are emptied
-- ('releaseViewerFrame'), and the rebuilt project fills them again,
-- so they stay where they are, in front of or behind the others.
-- Emptied windows that nothing took are destroyed afterwards.
--
-- The new project's viewers are compiled first, before anything is
-- replaced, on another thread (with a window saying so if that's slow:
-- see "UI.Loading"). If the user cancels that, the project stays as it
-- was, and this says so ('False').
replaceProject :: Backend -> Window a -> WxLayout -> Variable Ensemble -> UI
               -> [Window ()] -> Ensemble -> IO Bool
replaceProject backend projectWindow wxLayout currentEnsemble ui keep newEns = do
  name <- fromMaybe "the project" . mdTitle <$> (getDynamic . ensembleMetadata =<< getDynamic currentEnsemble)
  whileLoading name (prepareEnsemble backend newEns) >>= \case
    Nothing -> pure False
    Just prepared -> True <$ replaceWith prepared
 where
 replaceWith prepared = do
  rememberLayout wxLayout
  titles <- getDynamic (ensembleViewers newEns)
              >>= mapM (\cv -> fromRight "" <$> getDynamic (dyn (cvTitle cv)))
  keptViewers <- catMaybes <$> mapM (releaseViewerFrame wxLayout) titles
  keptConfig <- releaseConfigFrame wxLayout
  let kept = keep ++ map objectCast keptViewers ++ map objectCast (maybeToList keptConfig)
  windowChildren projectWindow >>= mapM_ windowDestroy . filter (`notElem` kept)
  oldEns <- getDynamic currentEnsemble
  carryOverProjectState oldEns newEns
  setValue' currentEnsemble newEns
  showEnsemble ui prepared
  discardSpareFrames wxLayout

-- | Rebuild a project from its script with changed colorizer choices
-- (by viewer title), then run @after@ on the rebuilt project. The new
-- project is built at once, so that a choice that doesn't work can be
-- reported (as 'Left', leaving the project as it was); but its windows
-- replace the old ones just afterwards, from a one-shot timer, since
-- this is called from one of the windows it replaces (from a viewer's
-- menu, say). An open script editor stays open, with its draft.
rebuildColorizers :: Backend -> Window a -> ScriptEditorHooks -> WxLayout -> Variable Ensemble -> UI
                  -> (Map.Map String String -> Map.Map String String)
                  -> (Ensemble -> IO ())
                  -> IO (Either String ())
rebuildColorizers backend projectWindow ScriptEditorHooks{sehEditor, sehKeep} wxLayout currentEnsemble ui change after = do
  prj <- getDynamic currentEnsemble
  src <- getDynamic (ensembleSourceScript prj)
  choices <- change <$> getDynamic (ensembleColorizerChoices prj)
  buildEnsembleWithColorizers choices src >>= \case
    Left err -> pure (Left (formatScriptError src err))
    Right newEns -> do
      replacer <- timer projectWindow [ interval := 1 ]
      set replacer [ on command := do
                       set replacer [ enabled := False ]
                       keep <- (++) <$> (maybeToList <$> readIORef sehEditor) <*> sehKeep
                       replaced <- replaceProject backend projectWindow wxLayout currentEnsemble ui keep newEns
                       when replaced (after newEns) ]
      pure (Right ())
