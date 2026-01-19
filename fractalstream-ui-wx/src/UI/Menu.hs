module UI.Menu
  ( makeMenuBar
  ) where

import FractalStream.Prelude hiding (get)
import FractalStream.Metadata

import UI.ProjectActions
import Data.DynamicValue

import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore.WxcClassesMZ (menuDestroyByItem)
import Graphics.UI.WXCore.Events
import Control.Concurrent.MVar

-- | Make a standard menu bar for `f`, plus any additional
-- menus we are given. Apparently this should be run after
-- setting the layout of `f`?
makeMenuBar :: ProjectActions -> Frame a -> [Menu ()] -> IO ()
makeMenuBar ProjectActions{..} f addlMenus = do

  let getProject verb k = maybe (pure ()) k =<<
        fileOpenDialog objectNull True True
           (verb ++ " a FractalStream project")
           [ ("FractalStream project files", ["*.yaml"])
           , ("All files", ["*.*"])
           ] "" ""

  -- Build the project menu
  prj <- menuPane [ text := "&Project" ]
  menuItem prj [ text := "New project\tCtrl+N"
               , help := "Create a new FractalStream project"
               , on command := projectNew ]
  menuItem prj [ text := "Open project\tCtrl+O"
               , help := "Open an existing FractalStream project"
               , on command := getProject "Open" projectOpen ]
  menuItem prj [ text := "Edit project\tCtrl+E"
               , help := "Modify an existing FractalStream project"
               , on command := getProject "Edit" projectEdit ]
  menuLine prj

  isActive <- newMVar True
  windowAddOnDelete f (modifyMVar_ isActive (pure . const False))

  sessionItems <- variable [ value := [] ]

  let updateSessionMenuItems sessions = do
        forM_ sessions $ \s@SessionInfo{..} -> do
          name <- getDynamic sessionName
          getDynamic sessionVisible >>= \case
            True  -> do
              mis <- get sessionItems value
              mi <- menuItem prj [ text := "Hide " ++ name
                                 , on command := hideSession s ]
              set sessionItems [ value := mi : mis ]

            False -> do
              mis <- get sessionItems value
              mi <- menuItem prj [ text := "Show " ++ name
                                 , on command := showSession s ]
              set sessionItems [ value := mi : mis ]

      destroySessionMenuItems = do
        mis <- get sessionItems value
        set sessionItems [ value := [] ]
        mapM_ (menuDestroyByItem prj) mis

  getDynamic activeSessions >>= updateSessionMenuItems
  stopWatchingSessions <- watchDynamic activeSessions $ \newSessions -> do
    active <- readMVar isActive
    when active $ do
      destroySessionMenuItems
      updateSessionMenuItems newSessions

  _quit <- menuQuit prj [] -- text := "Quit FractalStream" ]

  -- Build the help menu
  hlp   <- menuHelp      [ text := "&Help" ]
  about <- menuAbout hlp [ text := "About FractalStream" ]

  let menuBarItems = concat [ [prj], addlMenus, [hlp] ]

  set f [ menuBar := menuBarItems
        , on (menu about) :=
          infoDialog f "About FractalStream" $ unlines
          ("Contributors:" : contributors ++
            ["", "Build info:", gitBranch ++ "@" ++ take 8 gitHash])
        , on closing :~ (>> stopWatchingSessions)
        ]
