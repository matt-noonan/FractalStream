
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
import Actor.Metadata (ProjectMetadata(..))
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

  let -- A brand-new project has no file of its own yet, and nothing
      -- worth looking at until the user writes one -- so, unlike
      -- `projectOpen`, this skips straight to the whole-project script
      -- editor instead of calling `runEnsemble`, with `editProjectScript`'s
      -- own "Rebuild" button as the only way any viewer ever gets
      -- built. `newProjectScript` is a fixed, known-valid starting
      -- point (a project needs at least one `@viewer`), so the
      -- `either error id` here can only ever fire from a programming
      -- mistake in that constant, not from anything the user did.
      projectNew = do
        projectWindow <- frame [ visible := False ]
        prj <- either (error . formatScriptError newProjectScript) id
               <$> buildEnsembleFromScript newProjectScript
        currentEnsemble <- newVariable prj
        wxLayout <- newWxLayout (WxProperties mempty Nothing)

        sessionName <- newVariable "Untitled"
        sessionFile <- newVariable Nothing
        let sessionHandle = SessionHandle projectWindow
        sessionVisible <- newVariable True
        sessionUnsaved <- newVariable True
        let sessionSave = save currentEnsemble wxLayout sessionFile sessionName projectWindow sessionUnsaved
            ui = viewProject (objectCast projectWindow) wxLayout (makeMenuBar ProjectActions{..})
                   (projectMenuActions currentEnsemble sessionUnsaved sessionSave
                      (editProjectScript backend projectWindow wxLayout currentEnsemble ui))
            si = SessionInfo{..}
        modifyValue activeSessions (si :)
        editProjectScript backend projectWindow wxLayout currentEnsemble ui

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
      -- yet, and a successful save makes the chosen file the session's
      -- own from then on.
      save currentEnsemble wxLayout sessionFile sessionName f isUnsaved = do
        (dir, name) <- maybe ("", "Untitled.fs") (\p -> (takeDirectory p, takeFileName p))
                       <$> getDynamic sessionFile
        result <- fileSaveDialog f True True "Save FractalStream project"
                  projectFileTypes dir name
        case result of
          Nothing -> pure ()
          Just file -> do
            prj <- getDynamic currentEnsemble
            props <- captureLayout wxLayout
            modifyValue (ensembleFrontendProperties prj) (writeWxProperties props)
            encodeEnsemble prj >>= BS.writeFile file
            setValue isUnsaved False
            setValue sessionFile (Just file)
            setValue sessionName file

      -- Setting the preview or editing the project's info changes what
      -- gets saved, unlike moving windows around, so it marks the
      -- project as having unsaved changes.
      projectMenuActions currentEnsemble sessionUnsaved pmSave pmEditScript =
        ProjectMenuActions
          { pmSave, pmEditScript
          , pmEditInfo = \owner -> editProjectInfo owner currentEnsemble (setValue sessionUnsaved True)
          , pmSetPreview = \png -> do
              prj <- getDynamic currentEnsemble
              modifyValue (ensembleMetadata prj) (\md -> md { mdPreview = Just png })
              setValue sessionUnsaved True
          }

      projectOpen = \yamlFile -> do

        projectWindow <- frame [ visible := False ]
        let withRecoveryActions
              = (`catch` badProjectFile projectWindow yamlFile)
              . (`catch` errorCalled projectWindow)
              . (`catch` badYaml projectWindow yamlFile)

        withRecoveryActions $ do

          prj <- either error id <$> parseEnsembleFromFile yamlFile
          currentEnsemble <- newVariable prj
          -- A malformed `wx-properties:` only costs the remembered
          -- window layout, not the whole project, so it's reported
          -- and then ignored rather than refusing to open the file.
          wxProps <- readWxProperties <$> getDynamic (ensembleFrontendProperties prj) >>= \case
            Right props -> pure props
            Left err -> do
              putStrLn ("Ignoring unreadable `wx-properties:` in " ++ yamlFile ++ ": " ++ err)
              pure (WxProperties mempty Nothing)
          wxLayout <- newWxLayout wxProps

          sessionName <- newVariable yamlFile
          sessionFile <- newVariable (Just yamlFile)
          let sessionHandle = SessionHandle projectWindow
          sessionVisible <- newVariable True
          sessionUnsaved <- newVariable False
          let sessionSave = save currentEnsemble wxLayout sessionFile sessionName projectWindow sessionUnsaved
              -- `ui` and `editProjectScript` refer to each other: a
              -- rebuild needs a `UI` to hand `runEnsemble` for the
              -- *next* round of viewer windows, and that same `UI`
              -- needs the rebuild action for its own menus. Fine in
              -- Haskell as an ordinary lazy `let` -- `editProjectScript`
              -- only forces `ui` when the user actually picks the menu
              -- item, by which point it's long since finished building.
              ui = viewProject (objectCast projectWindow) wxLayout (makeMenuBar ProjectActions{..})
                     (projectMenuActions currentEnsemble sessionUnsaved sessionSave
                        (editProjectScript backend projectWindow wxLayout currentEnsemble ui))

              si = SessionInfo{..}
          modifyValue activeSessions (si :)
          runEnsemble backend ui prj

      closeSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> close f

      hideSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> do
          windowChildren f >>= mapM_ (\child -> set child [visible := False])
          mapM_ (markVisible sessionHandle False) =<< getDynamic activeSessions

      showSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> do
          windowChildren f >>= mapM_ (\child -> set child [visible := True])
          mapM_ (markVisible sessionHandle True) =<< getDynamic activeSessions

      markVisible h yn s
        | sessionHandle s == h = setValue (sessionVisible s) yn
        | otherwise = pure ()

  welcome ProjectActions{..}

-- | The starting point for "New project": the smallest script that's
-- actually valid (an ensemble needs at least one @\@viewer@), so a
-- brand-new project's editor opens with something to immediately
-- rebuild and see, rather than empty text.
newProjectScript :: String
newProjectScript = unlines
  [ "@viewer \"z plane\""
  , ""
  , "# Put your script here"
  , ""
  , "if stuck:"
  , "  color <- black"
  , "else:"
  , "  color <- rainbow(iterations / 20)"
  ]

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
