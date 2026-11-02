module UI.Menu
  ( makeMenuBar
  , ThisProject(..)
  , distinctNames
  ) where

import Foreign.Marshal.Array (withArray)
import Graphics.UI.WXCore.WxcClassesAL (acceleratorEntryCreate, acceleratorTableCreate)
import Graphics.UI.WXCore.WxcClassesMZ (windowSetAcceleratorTable)
import Graphics.UI.WXCore.Events (evtHandlerOnMenuCommand)
import Graphics.UI.WXCore.WxcTypes (withObjectRef)
import System.Info (os)
import qualified Data.Map as Map
import UI.Widgets (wxWatchDynamic1)
import Graphics.UI.WXCore.WxcDefs (wxID_PREFERENCES, wxITEM_NORMAL, wxACCEL_CTRL)
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
-- | What a window that belongs to a project can do to that project,
-- from its menu bar (where, unlike in a viewer's own pop-up menu, a
-- menu item's keyboard shortcut actually works).
data ThisProject = ThisProject
  { tpSave       :: IO ()
  , tpEditScript :: IO ()
  , tpEditInfo   :: Window () -> IO ()
  , tpClose      :: IO ()
  }

-- | Give a window its menu bar. On Linux and Windows, where a menu bar
-- is part of each window rather than one at the top of the screen, a
-- project's own windows don't get one (it would only repeat the
-- library browser's in every viewer and configuration window); they
-- just get their project's keyboard shortcuts ('projectShortcuts').
makeMenuBar :: ProjectActions -> Maybe ThisProject -> Frame a -> [Menu ()] -> IO ()
makeMenuBar actions thisProject f addlMenus = case thisProject of
  Just project | os /= "darwin" -> projectShortcuts project f
  _ -> menuBarFor actions thisProject f addlMenus

-- | Keyboard shortcuts for a project's commands, for a window without a
-- menu bar to hold them: Ctrl+S to save, Ctrl+E to edit the script,
-- Ctrl+I for the project's info, and Ctrl+W to close it.
projectShortcuts :: ThisProject -> Frame a -> IO ()
projectShortcuts ThisProject{..} f = do
  entries <- forM [ ('S', tpSave), ('E', tpEditScript)
                  , ('I', tpEditInfo (objectCast f)), ('W', tpClose) ] $ \(letter, action) -> do
    commandId <- idCreate
    evtHandlerOnMenuCommand f commandId action
    acceleratorEntryCreate wxACCEL_CTRL (fromEnum letter) commandId
  -- wxc wants an array of pointers to the entries.
  let withPointers [] k = k []
      withPointers (e : es) k =
        withObjectRef "projectShortcuts" e $ \p -> withPointers es (k . (p :))
  table <- withPointers entries $ \pointers ->
    withArray pointers (acceleratorTableCreate (length pointers))
  windowSetAcceleratorTable f table

menuBarFor :: ProjectActions -> Maybe ThisProject -> Frame a -> [Menu ()] -> IO ()
menuBarFor ProjectActions{..} thisProject f addlMenus = do


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

  -- In a project's own windows: that project's commands.
  forM_ thisProject $ \ThisProject{..} -> do
    menuLine prj
    menuItem prj [ text := "Save...\tCtrl+S", on command := tpSave ]
    menuItem prj [ text := "Edit Project Script...\tCtrl+E", on command := tpEditScript ]
    menuItem prj [ text := "Project Info...\tCtrl+I", on command := tpEditInfo (objectCast f) ]
    menuItem prj [ text := "Close\tCtrl+W", on command := tpClose ]



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
