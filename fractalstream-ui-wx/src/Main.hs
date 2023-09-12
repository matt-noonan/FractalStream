{- |
Module       : Main
Description  : Main entry point into FractalStream
-}
module Main where

import Actor.Ensemble
import Actor.Configuration
import Actor.Layout
import Actor.Viewer.Complex
import Actor.Tool
import Actor.Event
import UI.Definition (wxUI)
import Backend.LLVM (withViewerCode', withJIT)

import Graphics.UI.WX hiding (Vertical, Horizontal)
import qualified Graphics.UI.WXCore.Events as WX
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.WxcDefs
import qualified Data.Yaml as YAML

import Control.Monad
import qualified Data.Map as Map
import Data.IORef
import Data.Word


main :: IO ()
main = wxMain

wxMain :: IO ()
wxMain = withJIT $ \jit -> start $ do
  let getProject = do
        prj <- fileOpenDialog objectNull True True
               "Open a FractalStream 2 project"
               [ ("FractalStream 2 project files", ["*.yaml"])
               , ("All files", ["*.*"])
               ] "" ""
        maybe getProject pure prj

  yamlFile <- getProject

  -- TODO: verify that the code for each viewer, tool, etc works properly
  --       with the splices declared by the setup config. e.g. all code
  --       typechecks with the splice's types, each splice's environment
  --       is contained in the actual code environment at each use, etc.
  --
  --       If the ensemble passes this verification, then the end-user
  --       should not be able to cause a compilation failure via the
  --       UI.
  if True
    then do
      ensemble <- YAML.decodeFileThrow yamlFile
      let jitter = ComplexViewerCompiler (withViewerCode' jit)
      runEnsemble jitter wxUI ensemble
    else do
      editProject yamlFile

editProject :: FilePath -> IO ()
editProject yamlFile = do
  Ensemble{..} <- YAML.decodeFileThrow yamlFile

  print Ensemble{..}

  f <- frame [ text := "Editing project " ++ yamlFile
             , on resize := propagateEvent
             ]

  let splitter pp props0 = feed2 props0 (wxSP_3D .+.
                                         wxSP_3DBORDER .+.
                                         wxSP_3DSASH .+.
                                         wxSP_LIVE_UPDATE) $
        initialContainer $ \i r -> \props flags  ->
        do p <- splitterWindowCreate pp i r flags
           windowSetFocus p
           set p props
           return p

  sw <- splitter f [ visible := True ]

  pf <- panel sw []
  p0 <- panel pf [ ]

  tc <- treeCtrl sw [ text := "test"
                    , visible := True ]

  treeItemPanel <- newIORef []
  currentPanel <- newIORef p0

  do
    let addItem' i title mkPanel = do
          it <- treeCtrlAppendItem tc i title (-1) (-1) objectNull
          let tp = do
                p <- panel pf []
                mkPanel it p
                pure p
          modifyIORef' treeItemPanel ((it, tp):)
          pure it
        addItem i title = addItem' i title (\_ _ -> pure ())

    treeCtrlAddRoot tc "Project" (-1) (-1) objectNull
    root <- treeCtrlGetRootItem tc

    let tcFromLayout r0 teName =
         let go r = \case
               Vertical xs -> forM_ xs (go r)
               Horizontal xs -> do
                 sbs <- addItem r "side-by-side"
                 forM_ xs (go sbs)
               Panel s x -> do
                 g <- addItem r ("group \"" ++ s ++ "\"")
                 go g x
               Tabbed ts -> do
                 tb <- addItem r "tab group"
                 forM_ ts $ \(t, x) -> do
                   i <- addItem tb ("Tab \"" ++ t ++ "\"")
                   go i x
               TextBox _ (Dummy YamlVar{..}) -> do
                 void $ addItem r (teName ++ " for \"" ++ varVariable ++ "\"")
               CheckBox _ (Dummy YamlVar{..}) -> do
                 void $ addItem r ("Checkbox for \"" ++ varVariable ++ "\"")
               ColorPicker _ (Dummy YamlVar{..}) -> do
                 void $ addItem r ("Color picker for \"" ++ varVariable ++ "\"")
         in go r0

    whenJust ensembleSetup $ \Configuration{..} -> do
      let title x = "Setup \"" ++ x ++ "\""
      i <- addItem' root (title coTitle) $ \this p -> do
        te <- textEntry p [ text := coTitle
                          , processEnter := True
                          ]
        set te [ on command := do
                   newText <- get te text
                   treeCtrlSetItemText tc this (title newText)
               ]
        set p [ layout := fill $ row 5
                  [ margin 3 (label "Title of setup window: ")
                  , hfill (widget te)] ]

      tcFromLayout i "Expression" coContents

    whenJust ensembleConfiguration $ \Configuration{..} -> do
      let title x = "Configuration \"" ++ x ++ "\""
      i <- addItem' root (title coTitle) $ \this p -> do
        te <- textEntry p [ text := coTitle
                          , processEnter := True ]
        set te [ on command := do
                   newText <- get te text
                   treeCtrlSetItemText tc this (title newText)
               ]
        set p [ layout := fill $ row 5
                  [ margin 3 (label "Title of configuration window: ")
                  , hfill (widget te)] ]
      tcFromLayout i "Value" coContents


    forM_ ensembleViewers $ \ComplexViewer{..} -> do
      let title x = "ℂ viewer \"" ++ x ++ "\""
      v <- addItem' root (title cvTitle) $ \this p -> do
        te <- textEntry p [ text := cvTitle
                          , processEnter := True
                          ]
        set te [ on command := do
                   newText <- get te text
                   treeCtrlSetItemText tc this (title newText)
               ]
        ce <- codeEditor f p cvCode
        set p [ layout := hfill $ column 5 $
                [ fill $ row 5
                  [ margin 3 (label "Title of complex viewer: ")
                  , hfill (widget te) ]
                , hglue
                , fill (margin 10 $ widget ce)
                ] ]

      void $ addItem v "Default navigation tool"
      forM_ cvTools $ \(ComplexTool ParsedTool{..}) -> do
        t <- addItem v ("\"" ++ tiName ptoolInfo ++ "\" tool")
        whenJust ptoolConfig $ \Configuration{..} -> do
          i <- addItem t ("Tool configuration \"" ++ coTitle ++ "\"")
          tcFromLayout i "Expression" coContents
        let ParsedEventHandlers{..} = ptoolEventHandlers
        whenJust pehOnClick $ \(xCoord, yCoord, code) -> do
          addItem' t "Click event handler" $ \_ p -> do
            cb <- checkBox p [ text := "Use viewer coordinate as click coordinate"
                             , checkable := True
                             , checked := True ]
            teX <- textEntry p [ text := xCoord
                               , enabled := False ]
            teY <- textEntry p [ text := yCoord
                               , enabled := False ]
            ce <- codeEditor f p code
            set p [ layout := fill $ column 5
                    [ fill (widget cb)
                    , hglue
                    , fill $ row 5 [ widget teX, widget teY ]
                    , hglue
                    , fill (margin 10 $ widget ce) ]]
        whenJust pehOnDoubleClick $ \(xCoord, yCoord, code) -> do
          addItem' t "Double click event handler" $ \_ p -> do
            cb <- checkBox p [ text := "Use viewer coordinate as double-click coordinate"
                             , checkable := True
                             , checked := True ]
            teX <- textEntry p [ text := xCoord
                               , enabled := False ]
            teY <- textEntry p [ text := yCoord
                               , enabled := False ]
            ce <- codeEditor f p code
            set p [ color := rgb (0x80 :: Word8) 0 0
                  , layout := fill $ column 5
                    [ expand (widget cb)
                    , expand (row 5 [ widget teX, widget teY ])
                    , expand (widget ce) ]]
        whenJust pehOnDrag $ \_ -> do
          addItem' t "Drag event handler" $ \_ p -> do
            cb <- checkBox p [ text := "Use viewer coordinate as dragged-to coordinate"
                             , checkable := True
                             , checked := True ]
            set p [ layout := widget cb ]
        whenJust pehOnDragDone $ \_ -> do
          addItem' t "Drag finsihed event handler" $ \_ p -> do
            cb <- checkBox p [ text := "Use viewer coordinate as final drag coordinate"
                             , checkable := True
                             , checked := True ]
            set p [ layout := widget cb ]
        forM_ (Map.toList pehOnTimer) $ \(tmr, _) -> do
          addItem t ("Timer event handler for \"" ++ tmr ++ "\"")
          pure ()
        whenJust pehOnRefresh $ \_ -> do
          addItem t "View refresh event handler"
          pure ()
        whenJust pehOnActivated $ \_ -> do
          addItem t "Tool activation event handler"
          pure ()
        whenJust pehOnDeactivated $ \_ -> do
          addItem t "Tool deactivation event handler"
          pure ()

  let updatePanelForSelection = do
        i <- treeCtrlGetSelection tc
        fmap (filter ((== i) . fst)) (readIORef treeItemPanel) >>= \case
          [(_, mkPanel)] -> do
            sash <- splitterWindowGetSashPosition sw
            p <- mkPanel
            set pf [ layout := row 5 []]
            oldP <- readIORef currentPanel
            cs <- get oldP clientSize
            objectDelete oldP
            set p [ clientSize := cs ]
            writeIORef currentPanel p
            set pf [ layout := fill (stretch $ widget p) ]
            refit f
            splitterWindowSetSashPosition sw sash True
          _ -> pure () -- hmmmm...
        propagateEvent

  WX.windowOnEvent tc [wxEVT_COMMAND_TREE_SEL_CHANGED]
    updatePanelForSelection (const updatePanelForSelection)

  set pf [ layout := fill (stretch $ widget p0) ]
  splitterWindowSetBorderSize sw 20
  set f [ layout := fill (vsplit sw 5 200 (margin 5 $ fill (widget tc))
                           (margin 5 $ fill (stretch $ widget pf))) ]

whenJust :: Maybe a -> (a -> IO b) -> IO ()
whenJust mx f = maybe (pure ()) (void . f) mx

codeEditor :: Frame () -> Window a -> String -> IO (StyledTextCtrl ())
codeEditor ce cep code = do
    stc <- styledTextCtrl cep [ clientSize := sz 100 100 ]
    styledTextCtrlSetMarginWidth stc 0 30
    styledTextCtrlSetMarginWidth stc 1 0
    styledTextCtrlSetMarginWidth stc 2 0
    -- see Style Definition at https://www.scintilla.org/ScintillaDoc.html#Styling
    lum <- do
      col <- get ce bgcolor
      let rc :: Float = colorRed col / 255
          gc = colorGreen col / 255
          bc = colorBlue col / 255
      pure (0.299 * rc + 0.587 * gc + 0.114 * bc)
    if lum > 0.5
      then do
        styledTextCtrlStyleSetSpec stc  0 "fore:#000000,back:#f8f8f8"
        styledTextCtrlStyleSetSpec stc 32 "fore:#000000,back:#f8f8f8"
        styledTextCtrlStyleSetSpec stc 33 "fore:#808080,back:#f0f060"
        styledTextCtrlSetCaretLineBackground stc (rgb 240 240 (255 :: Word8))
      else do
        styledTextCtrlStyleSetSpec stc  0 "fore:#dbdbdb,back:#14191e"
        styledTextCtrlStyleSetSpec stc 32 "fore:#dbdbdb,back:#14191e"
        styledTextCtrlStyleSetSpec stc 33 "fore:#a0a0a0,back:#101040"
        styledTextCtrlSetCaretLineBackground stc (rgb 0x20 0x30 (0x38 :: Word8))

    styledTextCtrlStyleSetFaceName stc 0 "Monaco"
    -- Set the minimum size, or else Scintilla will default to 2000 pixels(!)
    styledTextCtrlSetScrollWidth stc 100
    styledTextCtrlSetCaretLineVisible stc True

    styledTextCtrlSetUseTabs stc False
    styledTextCtrlSetTabWidth stc 4
    styledTextCtrlSetIndent stc 4
    styledTextCtrlSetTabIndents stc True
    styledTextCtrlSetBackSpaceUnIndents stc True
    styledTextCtrlSetIndentationGuides stc True
    styledTextCtrlSetViewWhiteSpace stc 3

    styledTextCtrlSetText stc code
    pure stc
