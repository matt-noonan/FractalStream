module UI.Menu
  ( makeMenuBar
  , distinctNames
  ) where

import qualified Data.Map as Map
import UI.Widgets (wxWatchDynamic1)
import Graphics.UI.WXCore.WxcDefs (wxID_PREFERENCES, wxITEM_NORMAL)
import FractalStream.Prelude hiding (get)
import FractalStream.Metadata

import UI.ProjectActions
import UI.Session
import Data.DynamicValue

import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore.WxcClassesMZ (menuDestroyByItem)


-- | Make a standard menu bar for `f`, plus any additional
-- menus we are given. Apparently this should be run after
-- setting the layout of `f`?
makeMenuBar :: ProjectActions -> Frame a -> [Menu ()] -> IO ()
makeMenuBar ProjectActions{..} f addlMenus = do

  let getProject verb k = maybe (pure ()) k =<<
        fileOpenDialog objectNull True True
           (verb ++ " a FractalStream project")
           projectFileTypes "" ""

  -- Build the project menu
  prj <- menuPane [ text := "&Project" ]
  menuItem prj [ text := "New project\tCtrl+N"
               , help := "Create a new FractalStream project"
               , on command := projectNew ]
  menuItem prj [ text := "Open project\tCtrl+O"
               , help := "Open an existing FractalStream project"
               , on command := getProject "Open" projectOpen ]
  menuItem prj [ text := "Open from library...\tCtrl+L"
               , help := "Browse the projects that come with FractalStream"
               , on command := showLibrary ]
  -- The standard preferences id, so that on macOS it goes in the
  -- application menu, where people look for it.
  prefs <- menuItemEx prj wxID_PREFERENCES "&Preferences...\tCtrl+," wxITEM_NORMAL []


  menuLine prj


  -- Showing, hiding, and closing open projects: a submenu each, listing
  -- the projects it applies to (and disabled when there are none), so
  -- that many open projects don't crowd the menu itself.
  showMenu  <- menuPane []
  hideMenu  <- menuPane []
  closeMenu <- menuPane []
  showItem  <- menuSub prj showMenu  [ text := "Show project" ]
  hideItem  <- menuSub prj hideMenu  [ text := "Hide project" ]
  closeItem <- menuSub prj closeMenu [ text := "Close project" ]
  projectItems <- variable [ value := [] ]

  let -- Every open project (in the order they were opened), with its
      -- name and whether it's shown: any change to these rebuilds the
      -- submenus.
      sessionStates = do
        sessions <- reverse <$> dyn activeSessions
        forM sessions $ \s -> (s,,) <$> dyn (sessionName s) <*> dyn (sessionVisible s)

      rebuildProjectMenus states = do
        mapM_ (\(sub, mi) -> menuDestroyByItem sub mi) =<< get projectItems value
        let itemNames = distinctNames [ name | (_, name, _) <- states ]
        newItems <- fmap concat . forM (zip states itemNames) $ \((s, _, shown), itemName) -> do
          toggle <- if shown
            then (hideMenu,) <$> menuItem hideMenu [ text := itemName, on command := hideSession s ]
            else (showMenu,) <$> menuItem showMenu [ text := itemName, on command := showSession s ]
          closer <- (closeMenu,) <$> menuItem closeMenu [ text := itemName, on command := closeSession s ]
          pure [toggle, closer]

        set projectItems [ value := newItems ]
        set showItem  [ enabled := any (\(_, _, shown) -> not shown) states ]
        set hideItem  [ enabled := any (\(_, _, shown) -> shown) states ]
        set closeItem [ enabled := not (null states) ]

  -- Rebuilt on the main thread ('wxWatchDynamic'): watching a value
  -- made of several variables runs its action on another thread, and
  -- AppKit (at least) throws if a menu is changed from any thread but
  -- the main one. 'wxWatchDynamic' also stops when `f` is destroyed.
  wxWatchDynamic1 f sessionStates rebuildProjectMenus


  _quit <- menuQuit prj [] -- text := "Quit FractalStream" ]

  -- Build the help menu
  hlp   <- menuHelp      [ text := "&Help" ]
  about <- menuAbout hlp [ text := "About FractalStream" ]

  let menuBarItems = concat [ [prj], addlMenus, [hlp] ]

  set f [ menuBar := menuBarItems
        , on (menu prefs) := showPreferences
        , on (menu about) :=
          infoDialog f "About FractalStream" $ unlines
          ( ("FractalStream " ++ gitTag) : "" :
            "Contributors:" : map ("  " ++) contributors ++
            ["", "Build info:", gitBranch ++ "@" ++ take 8 gitHash])
        ]

-- | Names for a list of projects that tell them all apart: a name that
-- several share is numbered after its first, as in "Untitled",
-- "Untitled (2)", "Untitled (3)".
distinctNames :: [String] -> [String]
distinctNames = go Map.empty
  where
    go _ [] = []
    go seen (name : rest) =
      let n = Map.findWithDefault (0 :: Int) name seen + 1
      in (if n == 1 then name else name ++ " (" ++ show n ++ ")")
         : go (Map.insert name n seen) rest
