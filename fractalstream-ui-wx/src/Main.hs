
{-# language CPP #-}
{- |
Module       : Main
Description  : Main entry point into FractalStream
-}
module Main where

import UI.ProjectActions
import UI.Menu
import UI.ProjectViewer (viewProject, ProjectMenuActions(..))
import UI.ProjectInfo (editProjectInfo)
import Actor.Metadata (ProjectMetadata(..), noMetadata)
import Actor.SavedValues (readSavedValues, writeSavedValues)

import Control.Monad (void)
import UI.ProjectScriptEditor (editProjectScript)
import UI.Session
import UI.WxProperties
import Data.DynamicValue
import Actor.Ensemble

import qualified Data.Yaml as YAML

import UI.Welcome

import Backend
import Graphics.UI.WX --(start)
import Graphics.UI.WXCore.Draw (withFontStyle)
import Graphics.UI.WXCore.Frame (windowChildren)
import Graphics.UI.WXCore.WxcClasses ( styledTextCtrlStyleSetFont
                                     , styledTextCtrlSetText
                                     , wxcAppSetAppName )

import Control.Exception (Exception, catch, ErrorCall(..))
import qualified Data.ByteString as BS
import System.FilePath (takeDirectory, takeFileName)
import FractalStream.Library (LibraryEntry(..), projectsDirectory)
import FractalStream.Settings (loadSettings)
import UI.Preferences (preferencesWindow)

import System.Directory (createDirectoryIfMissing)

import Data.IORef
import qualified Graphics.UI.WXCore as WX



#if defined(mingw32_HOST_OS)
import Control.Exception (IOException, try)
import GHC.IO.Handle (hDuplicateTo)
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import System.IO ( IOMode(..), BufferMode(..), openFile, hSetEncoding, utf8
                 , hSetBuffering, stdout, stderr )
#endif

main :: IO ()
main = redirectOutputOnWindows >> runApp

-- | On Windows, FractalStream is a GUI program (linked with -mwindows), so
-- it has no console, and writing to stdout or stderr would throw. Send them
-- to FractalStream.log in the temporary directory instead (or, if that
-- can't be opened, nowhere).
redirectOutputOnWindows :: IO ()
#if defined(mingw32_HOST_OS)
redirectOutputOnWindows = do
  tmp <- getTemporaryDirectory
  logFile <- try (openFile (tmp </> "FractalStream.log") WriteMode)
             >>= either (\(_ :: IOException) -> openFile "NUL" WriteMode) pure
  hSetEncoding logFile utf8
  hSetBuffering logFile LineBuffering
  hDuplicateTo logFile stdout
  hDuplicateTo logFile stderr
#else
redirectOutputOnWindows = pure ()
#endif

runApp :: IO ()
runApp = withBackend $ \backend -> start $ do

  wxcAppSetAppName "FractalStream"

  activeSessions <- newVariable []
  libraryWindow <- newIORef Nothing
  preferencesFrame <- newIORef Nothing

  -- FractalStream's own settings: whatever in them can't be read is
  -- reported and left at its default.
  (initialSettings, settingsWarnings) <- loadSettings
  mapM_ (putStrLn . ("Settings: " ++)) settingsWarnings
  settings <- newVariable initialSettings

  let -- A new project starts from one of the library's starting points
      -- (including a blank one), chosen in the library browser.
      projectNew = libraryBrowser >>= id

      -- `currentEnsemble` holds whichever `Ensemble` the session is
      -- currently running -- fixed at open time, but replaced in place
      -- by a successful rebuild from `editProjectScript`, so `save`
      -- (and any later rebuild) always acts on the live one rather
      -- than a value that was only ever accurate at open time.
      --
      -- Window positions, sizes, and current views are saved along
      -- with the script, under `wx-properties:`. Changing them doesn't
      -- count as an unsaved change, though: they shift constantly, and
      -- being asked to save on every close just because a window was
      -- nudged would be more annoying than useful.
      --
      -- The dialog starts out on the session's own file, if it has one
      -- yet, or else on @suggestedName@ in the user's projects folder
      -- (made if need be), where the library browser will show it; and
      -- a successful save makes the chosen file the session's own from
      -- then on.
      save currentEnsemble wxLayout sessionFile suggestedName sessionName f isUnsaved = do
        (dir, name) <- getDynamic sessionFile >>= \case
          Just p -> pure (takeDirectory p, takeFileName p)
          Nothing -> do
            mine <- projectsFolder
            createDirectoryIfMissing True mine
            pure (mine, suggestedName)
        result <- fileSaveDialog f True True "Save FractalStream project"
                  projectFileTypes dir name
        case result of
          Nothing -> pure ()
          Just file -> do
            prj <- getDynamic currentEnsemble
            -- Until a project's script builds, it has no windows or
            -- configuration to read its layout and values from, so
            -- they're saved just as its file had them.
            when (ensembleBuilt prj) $ do
              props <- captureLayout wxLayout
              modifyValue (ensembleFrontendProperties prj) (writeWxProperties props)
              values <- changedValues prj
              modifyValue (ensembleFrontendProperties prj) (writeSavedValues values)

            encodeEnsemble prj >>= BS.writeFile file
            setValue isUnsaved False
            setValue sessionFile (Just file)
            setValue sessionName file

      -- Setting the preview or editing the project's info changes what
      -- gets saved, unlike moving windows around, so it marks the
      -- project as having unsaved changes.
      projectMenuActions currentEnsemble sessionUnsaved pmSave pmEditScript pmClose =
        ProjectMenuActions
          { pmSave, pmEditScript, pmClose

          , pmEditInfo = \owner -> editProjectInfo owner currentEnsemble (setValue sessionUnsaved True)
          , pmResetValues = getDynamic currentEnsemble >>= resetValues
          , pmSetPreview = \png -> do
              prj <- getDynamic currentEnsemble
              modifyValue (ensembleMetadata prj) (\md -> md { mdPreview = Just png })
              setValue sessionUnsaved True
          }

      projectOpen = \path ->
        openProject path (Just path) (takeFileName path) False (loadProjectFile path)

      -- A library project opens as a copy with no file of its own, so
      -- that saving it never tries to write into the app itself; the
      -- first save suggests the library project's own filename. A
      -- starting point opens as a new, untitled project instead, with
      -- the script editor open, ready to write. One of the user's own
      -- projects just opens, as itself.
      projectOpenLibrary = \LibraryEntry{..} ->
        let load = loadProjectBytes leTitle leContents
        in case leFile of
             Just path -> projectOpen path
             Nothing
               | leStartingPoint -> openProject "Untitled" Nothing "Untitled.fs" True load
               | otherwise -> openProject leTitle Nothing leFileName False load

      -- Open a project as a new session: @name@ is what to call it (in
      -- messages and the Project menu), @file@ the file it's saved to,
      -- if it has one yet, and @suggestedName@ what to suggest calling
      -- that file when it doesn't. A project that's @fromStartingPoint@
      -- doesn't keep the starting point's metadata (its title,
      -- description, and preview describe the starting point, not the
      -- project made from it), and opens with its script editor.
      openProject name file suggestedName fromStartingPoint load = do

        projectWindow <- frame [ visible := False ]
        let withRecoveryActions
              = (`catch` badProjectFile projectWindow name)
              . (`catch` errorCalled projectWindow)
              . (`catch` badYaml projectWindow name)

        withRecoveryActions $ do

          -- A project whose script doesn't build still opens, straight
          -- into its script editor (which shows what's wrong), so that
          -- it can be fixed; its viewers appear, with its saved layout
          -- and values, when the fixed script is rebuilt. Only a file
          -- that can't be read as a project at all is an error here.
          loaded <- either error id <$> load
          let (prj, scriptBuilds) = case loaded of
                Built ens -> (ens, True)
                BrokenScript ens _ -> (ens, False)
          when fromStartingPoint $ setValue' (ensembleMetadata prj) noMetadata
          -- Put back the configuration values the project was saved
          -- with, where they differ from its script's defaults, before
          -- anything is drawn with them. Like a malformed
          -- `wx-properties:`, one that no longer fits is reported and
          -- skipped rather than refusing to open the project.
          when scriptBuilds $
            readSavedValues <$> getDynamic (ensembleFrontendProperties prj) >>= \case
              Left err -> putStrLn ("Ignoring unreadable `values:` in " ++ name ++ ": " ++ err)
              Right saved -> applySavedValues prj saved >>= mapM_ (putStrLn . (("In " ++ name ++ ", ") ++))
          currentEnsemble <- newVariable prj
          -- A malformed `wx-properties:` only costs the remembered
          -- window layout, not the whole project, so it's reported
          -- and then ignored rather than refusing to open the file.
          wxProps <- readWxProperties <$> getDynamic (ensembleFrontendProperties prj) >>= \case
            Right props -> pure props
            Left err -> do
              putStrLn ("Ignoring unreadable `wx-properties:` in " ++ name ++ ": " ++ err)
              pure (WxProperties mempty Nothing)
          wxLayout <- newWxLayout wxProps

          sessionName <- newVariable name
          sessionFile <- newVariable file
          let sessionHandle = SessionHandle projectWindow
          sessionVisible <- newVariable True
          sessionUnsaved <- newVariable False
          -- Changing a configuration value changes what gets saved, so
          -- it marks the project as having unsaved changes -- following
          -- the project through rebuilds, which replace its ensemble.
          -- Rebuilding from an edited script (which replaces the
          -- project's ensemble) is a change to save, too.
          let trackValues ens = void (watchValues ens (setValue sessionUnsaved True))
          trackValues prj
          void (watchDynamic currentEnsemble $ \ens -> do
                  setValue sessionUnsaved True
                  trackValues ens)

          let sessionSave = save currentEnsemble wxLayout sessionFile suggestedName sessionName projectWindow sessionUnsaved
              -- `ui` and `editProjectScript` refer to each other: a
              -- rebuild needs a `UI` to hand `runEnsemble` for the
              -- *next* round of viewer windows, and that same `UI`
              -- needs the rebuild action for its own menus. Fine in
              -- Haskell as an ordinary lazy `let` -- `editProjectScript`
              -- only forces `ui` when the user actually picks the menu
              -- item, by which point it's long since finished building.
              ui = viewProject (objectCast projectWindow) wxLayout (makeMenuBar ProjectActions{..})
                     (projectMenuActions currentEnsemble sessionUnsaved sessionSave
                        (editProjectScript backend projectWindow showLibraryAfterClosing wxLayout currentEnsemble ui)
                        (closeSession si))

              si = SessionInfo{..}
          modifyValue activeSessions (si :)
          -- Once the project's window is gone (however it went), the
          -- project is too: the Project menu mustn't go on offering to
          -- show or hide it.
          previousOnDestroy <- WX.windowGetOnDestroy projectWindow
          WX.windowOnDestroy projectWindow (forgetSession sessionHandle >> previousOnDestroy)
          when scriptBuilds $ runEnsemble backend ui prj
          when (fromStartingPoint || not scriptBuilds) $
            editProjectScript backend projectWindow showLibraryAfterClosing wxLayout currentEnsemble ui

      -- Close a project (all its windows), first offering to save it if
      -- it has unsaved changes. Saving and then cancelling the save
      -- dialog, or answering Cancel, leaves it open. The question has
      -- no parent window: the project's own is hidden, and wx would
      -- show it again once a dialog over it closed.
      closeSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> do
          unsaved <- getDynamic sessionUnsaved
          proceed <- if not unsaved then pure True else do
            name <- getDynamic sessionName
            answer <- WX.messageDialog objectNull "Close project"
                        ("Save the changes to " ++ name ++ " before closing it?")
                        (WX.wxYES_NO .+. WX.wxCANCEL .+. WX.wxICON_QUESTION)
            if | answer == WX.wxID_YES -> sessionSave >> not <$> getDynamic sessionUnsaved
               | answer == WX.wxID_NO  -> pure True
               | otherwise             -> pure False
          when proceed $ do
            void (WX.windowClose f True)
            showLibraryAfterClosing


      hideSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> do
          windowChildren f >>= mapM_ (\child -> set child [visible := False])
          mapM_ (markVisible sessionHandle False) =<< getDynamic activeSessions

      showSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> do
          windowChildren f >>= mapM_ (\child -> set child [visible := True])
          mapM_ (markVisible sessionHandle True) =<< getDynamic activeSessions

      projectsFolder = getDynamic settings >>= projectsDirectory

      -- At most one Preferences window at a time.
      showPreferences = readIORef preferencesFrame >>= \case
        Just f -> WX.windowRaise f
        Nothing -> do
          f <- preferencesWindow settings
          writeIORef preferencesFrame (Just f)
          WX.windowOnDestroy f (writeIORef preferencesFrame Nothing)

      markVisible h yn s
        | sessionHandle s == h = setValue (sessionVisible s) yn
        | otherwise = pure ()

      forgetSession h = modifyValue activeSessions (filter ((/= h) . sessionHandle))


      -- There's at most one library browser at a time: showing it
      -- again just brings the open one to the front. Returns the
      -- action that switches it to the starting points.
      libraryBrowser = readIORef libraryWindow >>= \case
        Just (f, showStartingPoints) -> do
          WX.windowRaise f
          pure showStartingPoints
        Nothing -> do
          browser@(f, showStartingPoints) <- welcome ProjectActions{..}
          writeIORef libraryWindow (Just browser)
          WX.windowOnDestroy f (writeIORef libraryWindow Nothing)
          pure showStartingPoints

      showLibrary = void libraryBrowser

      -- On macOS, the menu bar is the key window's, so once a project's
      -- windows are gone, another has to show its menu bar, or there's
      -- none to speak of until the user clicks on a window. So the
      -- library browser is brought forward -- but that isn't enough by
      -- itself: wx (3.2) takes the menu bar down whenever a frame is
      -- hidden or destroyed, which the project's frames are only once
      -- the event that closed them is over, and it only puts a frame's
      -- menu bar up again when that frame is activated, which the
      -- browser, already in front by then, isn't. Re-attaching the
      -- browser's menu bar a moment later puts it up directly (wx does
      -- that for the frontmost window's).
      showLibraryAfterClosing = do
        showLibrary
        readIORef libraryWindow >>= mapM_ (\(b, _) -> do
          again <- timer b [ interval := 150 ]
          set again [ on command := do
                        set again [ enabled := False ]
                        WX.windowRaise b
                        menus <- WX.frameGetMenuBar b
                        WX.frameSetMenuBar b WX.objectNull
                        WX.frameSetMenuBar b menus ])


  showLibrary

badYaml :: Frame a -> FilePath -> YAML.ParseException -> IO ()
badYaml w path e = badProjectFile w path (BadProject "Bad YAML file" (show e))

badProjectFile :: Frame a -> FilePath -> BadProject -> IO ()
badProjectFile projectWindow path (BadProject why wher) =
  fatalErrorDialog projectWindow "Could not load project file" $ \p -> withFontStyle fontFixed $ \fnt -> do
    txt <- staticText p
        [ text := unlines
          [ "I was unable to load the project from " ++ path
          , "because " ++ why
          , ""
          ]]
    errTxt <- styledTextCtrl p []
    styledTextCtrlStyleSetFont errTxt 0 fnt
    styledTextCtrlSetText errTxt wher

    pure [ widget txt, expand $ widget errTxt ]

errorCalled :: Frame a -> ErrorCall -> IO ()
errorCalled projectWindow (ErrorCall msg) =
  fatalErrorDialog projectWindow "Internal error" $ \p -> withFontStyle fontFixed $ \fnt -> do
    txt <- staticText p
      [ text := unlines
        [ "Oh my."
        , ""
        , "There was an uncaught internal error:" ]]
    moreTxt <- styledTextCtrl p []
    styledTextCtrlStyleSetFont moreTxt 0 fnt
    styledTextCtrlSetText moreTxt msg
    yetMoreTxt <- staticText p
      [ text := unlines
        [ ""
        , "Perhaps you should report this at"
        , "https://github.com/matt-noonan/FractalStream/issues"
        ]
      ]
    pure [widget txt, expand $ widget moreTxt, widget yetMoreTxt]

fatalErrorDialog :: Frame a -> String -> (Panel () -> IO [Layout]) -> IO ()
fatalErrorDialog projectWindow title mkContents = do
  d <- dialog objectNull [ text := title ]
  p <- panel d []
  contents <- mkContents p
  ok <- button p [text := "OK"]
  set d [ layout := fill $ container p $ margin 15 $ floatCenter $
          column 5 (contents ++ [widget ok]) ]
  _ <- showModal d (\done -> set ok [on command := done Nothing ])
  close projectWindow

data BadProject = BadProject String String deriving Show
instance Exception BadProject
