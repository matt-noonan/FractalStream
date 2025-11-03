
{- |
Module       : Main
Description  : Main entry point into FractalStream
-}
module Main where

import Actor.Ensemble

import UI.ProjectActions
import UI.Menu
import UI.ProjectViewer (viewProject)
import UI.ProjectEditor (editProject)
import Data.DynamicValue

import Actor.Viewer.Complex (BadProject(..))

import qualified Data.Yaml as YAML

import UI.Welcome

import Backend
import Graphics.UI.WX --(start)
import Graphics.UI.WXCore.Draw (withFontStyle)
import Graphics.UI.WXCore.Frame (windowChildren)
import Graphics.UI.WXCore.WxcClasses ( styledTextCtrlStyleSetFont
                                     , styledTextCtrlSetText
                                     , wxcAppSetAppName )

import Control.Exception (catch, ErrorCall(..))

main :: IO ()
main = withBackend $ \complexViewerCompiler -> start $ do

  wxcAppSetAppName "FractalStream"

  sessions <- newUIValue []

  let projectNew = putStrLn "TODO"

      projectOpen = \yamlFile -> do
        projectWindow <- frame [ visible := False ]
        let withRecoveryActions
              = (`catch` badProjectFile projectWindow yamlFile)
              . (`catch` errorCalled projectWindow)
              . (`catch` badYaml projectWindow yamlFile)

        withRecoveryActions $ do

          prj <- either error id <$> parseTemplateFromFile yamlFile

          let si = SessionInfo
                { sessionName = yamlFile
                , sessionHandle = SessionHandle projectWindow
                , sessionVisible = True
                , sessionUnsaved = False }
          modifyUIValue sessions (si :)
          runTemplate complexViewerCompiler
            (viewProject (objectCast projectWindow) (makeMenuBar ProjectActions{..}))
            prj

      projectOpenTemplate = \name prj -> do
        projectWindow <- frame [ visible := False ]
        let withRecoveryActions
              = (`catch` badProjectFile projectWindow name)
              . (`catch` errorCalled projectWindow)
        withRecoveryActions $ do
          let si = SessionInfo
                { sessionName = name
                , sessionHandle = SessionHandle projectWindow
                , sessionVisible = True
                , sessionUnsaved = False }
          modifyUIValue sessions (si :)
          runTemplate complexViewerCompiler
            (viewProject (objectCast projectWindow) (makeMenuBar ProjectActions{..}))
            prj

      projectEdit = editProject

      activeSessions = SomeDynamic sessions

      closeSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> close f

      hideSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> do
          windowChildren f >>= mapM_ (\child -> set child [visible := False])
          modifyUIValue sessions (map $ markVisible sessionHandle False)

      showSession SessionInfo{..} = case sessionHandle of
        SessionHandle f -> do
          windowChildren f >>= mapM_ (\child -> set child [visible := True])
          modifyUIValue sessions (map $ markVisible sessionHandle True)

      markVisible h yn s
        | sessionHandle s == h = s { sessionVisible = yn }
        | otherwise = s

      editSession _ = putStrLn "TODO, editSession"

  welcome ProjectActions{..}

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
