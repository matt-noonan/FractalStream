
{- |
Module       : Main
Description  : Main entry point into FractalStream
-}
module Main where

import UI.ProjectActions
import UI.Menu
import UI.ProjectViewer (viewProject)
import UI.ProjectScriptEditor (editProjectScript)
import UI.Session
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

main :: IO ()
main = withBackend $ \backend -> start $ do

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

        sessionName <- newVariable "Untitled"
        let sessionHandle = SessionHandle projectWindow
        sessionVisible <- newVariable True
        sessionUnsaved <- newVariable True
        let sessionSave = save currentEnsemble projectWindow sessionUnsaved
            ui = viewProject (objectCast projectWindow) (makeMenuBar ProjectActions{..})
                   sessionSave (editProjectScript backend projectWindow currentEnsemble ui)
            si = SessionInfo{..}
        modifyValue activeSessions (si :)
        editProjectScript backend projectWindow currentEnsemble ui

      -- `currentEnsemble` holds whichever `Ensemble` the session is
      -- currently running -- fixed at open time, but replaced in place
      -- by a successful rebuild from `editProjectScript`, so `save`
      -- (and any later rebuild) always acts on the live one rather
      -- than a value that was only ever accurate at open time.
      save currentEnsemble f isUnsaved = do
        result <- fileSaveDialog f True True "Save FractalStream project"
                  [ ("FractalStream project files",["*.fs"]), ("All files", ["*.*"])] "" ".fs"
        case result of
          Nothing -> pure ()
          Just file -> do
            prj <- getDynamic currentEnsemble
            encodeEnsemble prj >>= BS.writeFile file
            setValue isUnsaved False

      projectOpen = \yamlFile -> do
        projectWindow <- frame [ visible := False ]
        let withRecoveryActions
              = (`catch` badProjectFile projectWindow yamlFile)
              . (`catch` errorCalled projectWindow)
              . (`catch` badYaml projectWindow yamlFile)

        withRecoveryActions $ do

          prj <- either error id <$> parseEnsembleFromFile yamlFile
          currentEnsemble <- newVariable prj

          sessionName <- newVariable yamlFile
          let sessionHandle = SessionHandle projectWindow
          sessionVisible <- newVariable True
          sessionUnsaved <- newVariable False
          let sessionSave = save currentEnsemble projectWindow sessionUnsaved
              -- `ui` and `editProjectScript` refer to each other: a
              -- rebuild needs a `UI` to hand `runEnsemble` for the
              -- *next* round of viewer windows, and that same `UI`
              -- needs the rebuild action for its own menus. Fine in
              -- Haskell as an ordinary lazy `let` -- `editProjectScript`
              -- only forces `ui` when the user actually picks the menu
              -- item, by which point it's long since finished building.
              ui = viewProject (objectCast projectWindow) (makeMenuBar ProjectActions{..})
                     sessionSave (editProjectScript backend projectWindow currentEnsemble ui)
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

      editSession _ = putStrLn "TODO, editSession"

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

runTemplate :: a -> b -> c -> IO ()
runTemplate _ _ _ = putStrLn "TODO: runTemplate"

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
