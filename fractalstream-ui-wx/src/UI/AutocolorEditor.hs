-- | A window for a project's autocolor groups (see "Actor.Autocolor"): pick a
-- group, and see, edit, add, and delete its points and their colors; lock
-- or unlock it; or clear it.
module UI.AutocolorEditor
  ( editAutocolor
  ) where

import FractalStream.Prelude hiding (get)

import Actor.Autocolor
import Data.Color (Color, colorToRGB, rgbToColor, grey)
import Data.DynamicValue
import Language.Type (TypeProxy(ComplexType, RealType))
import Language.Value.Parser (parseConstant)

import UI.Widgets (wxWatchDynamic, wxWatchDynamic1, onOwnDestroy)

import Graphics.UI.WX hiding (pt, when, grey, Color, Object, Layout)
import qualified Graphics.UI.WX as WX
import Graphics.UI.WXCore.Events
import Graphics.UI.WXCore.WxcClassesAL
import Graphics.UI.WXCore.WxcClassesMZ
import Graphics.UI.WXCore.Frame (frameDefaultStyle, windowChildren)
import Graphics.UI.WXCore.WxcClassTypes (ColourPickerCtrl)

import Data.IORef
import Control.Exception (SomeException, catch, displayException)
import System.IO (hPutStrLn, stderr)
import qualified Data.Map as Map

-- | Open the autocolor window, over the project's window. @changed@ is
-- run after each change made in it (to mark the project as changed), and
-- @closed@ once the window is gone.
editAutocolor :: Window a -> AutocolorState -> IO () -> IO () -> IO (Frame ())
editAutocolor owner st changed closed = do
  f <- frameEx frameDefaultStyle [ text := "Autocolor", visible := False ] owner
  p <- panel f []

  groupChoice <- choice p []
  lockBox <- checkBox p [ text := "Lock", tooltip := "Add no new points to this group" ]
  clearBtn <- button p [ text := "Clear all" ]
  toleranceEntry <- textEntry p [ text := "", processEnter := True
                                , tooltip := "How near a point has to be to an entry to get its color" ]
  -- (the miss color picker's action needs `selected`, so it's set below)
  missAction <- newIORef (\_ -> pure ())
  missPicker <- colorPicker p grey (\c -> readIORef missAction >>= ($ c))
  list <- scrolledWindow p [ scrollRate := sz 10 10 ]
  newPoint <- textEntry p [ text := "", processEnter := True ]
  addBtn <- button p [ text := "Add" ]
  status <- staticText p [ text := "" ]

  groupIds <- newIORef ([] :: [AutocolorGroupId])
  selected <- newIORef (Nothing :: Maybe AutocolorGroup)
  shownPoints <- newIORef (Nothing :: Maybe [(Double, Double)])
  -- Which build of the list is the current one: a row from an earlier
  -- build mustn't act. (Destroying a row's point field while it has the
  -- focus -- which on macOS it keeps when a button is clicked -- runs its
  -- "lost focus" commit, on a field that's half gone.)
  generation <- newIORef (0 :: Int)

  let say msg = set status [ text := msg ] >> windowReLayout f

      showTolerance g = do
        r <- getDynamic (agTolerance g)
        set toleranceEntry [ text := showAutocolorPoint (r, 0) ]

      commitTolerance = readIORef selected >>= mapM_ (\g -> do
        txt <- get toleranceEntry text
        old <- getDynamic (agTolerance g)
        case parseConstant RealType txt of
          Right r | r > 0 -> when (r /= old) (setAutocolorTolerance st g r >> madeChange)
          _ -> do
            showTolerance g
            say ("The tolerance has to be a positive number, not " ++ show txt ++ "."))

      readPoint :: String -> Either String (Double, Double)
      readPoint txt = case parseConstant ComplexType txt of
        Right (x :+ y) -> Right (x, y)
        Left _ -> Left ("Couldn't read " ++ show txt ++ " as a complex number.")

      madeChange = changed >> say ""

      -- One row of the list: the point, its color, and a Delete button.
      entryRow gen g (pt, cv) = do
        pointEntry <- textEntry list [ text := showAutocolorPoint pt, processEnter := True ]
        c0 <- getDynamic cv
        picker <- colorPicker list c0 $ \c -> setAutocolorEntryColor st g pt c >> madeChange
        deleteBtn <- button list [ text := "Delete" ]
        let current = (== gen) <$> readIORef generation
            commit = current >>= \ok -> when ok $ do
              txt <- get pointEntry text
              case readPoint txt of
                Left err -> do
                  set pointEntry [ text := showAutocolorPoint pt ]
                  say err
                Right pt'
                  | pt' == pt -> pure ()
                  | otherwise -> moveAutocolorEntry st g pt pt' >> madeChange >> showEntries
        set pointEntry [ on command := commit
                       , on focus := \gained -> unless gained commit ]
        set deleteBtn [ on command := current >>= \ok -> when ok $ do
                          deleteAutocolor st g (fst pt) (snd pt)
                          madeChange
                          showEntries ]
        pure (row 5 [ hfill (widget pointEntry), widget picker, widget deleteBtn ])

      -- (Re)build the list of the selected group's entries.
      showEntries = do
        gen <- atomicModifyIORef' generation (\n -> (n + 1, n + 1))
        windowChildren list >>= mapM_ windowDestroy
        readIORef selected >>= \case
          Nothing -> do
            writeIORef shownPoints Nothing
            set list [ layout := WX.empty ]
          Just g -> do
            table <- Map.toList <$> getDynamic (agTable g)
            writeIORef shownPoints (Just (map fst table))
            rows <- mapM (entryRow gen g) table
            -- (`column` can't be given no rows at all)
            set list [ layout := if null rows then WX.empty else margin 5 (column 3 rows) ]
        windowFitInside list
        windowReLayout f

      selectGroup ix = do
        ids <- readIORef groupIds
        groups <- autocolorGroups st
        let mg = do
              gId <- if ix >= 0 && ix < length ids then Just (ids !! ix) else Nothing
              Map.lookup gId groups
        writeIORef selected mg
        forM_ mg $ \g -> do
          getDynamic (agLocked g) >>= \b -> set lockBox [ checked := b ]
          showTolerance g
          (r, gr, b) <- colorToRGB <$> getDynamic (agMissColor g)
          colorPickerCtrlSetColour missPicker (rgb r gr b)
        mapM_ (\w -> set w [ enabled := isJust mg ])
              [ objectCast lockBox, objectCast clearBtn, objectCast newPoint, objectCast addBtn
              , objectCast toleranceEntry, objectCast missPicker :: Window () ]
        showEntries

      -- Keep the group selector up to date with the groups scripts add
      -- while they draw (the first time each one uses autocolor).
      syncGroups = do
        ids <- Map.keys <$> autocolorGroups st
        old <- readIORef groupIds
        when (ids /= old) $ do
          sel <- get groupChoice selection
          let keep = if sel >= 0 && sel < length old then Just (old !! sel) else Nothing
          writeIORef groupIds ids
          set groupChoice [ items := [ "Group " ++ show n | AutocolorGroupId n <- ids ] ]
          let ix = fromMaybe 0 (keep >>= \k -> lookup k (zip ids [0 ..]))
          unless (null ids) $ set groupChoice [ selection := ix ]
          selectGroup (if null ids then -1 else ix)
        when (null ids) $
          say "No viewer has used autocolor yet: a group appears here once one does."

      -- Keep the list up to date with the points scripts add while they
      -- draw: rebuilt only when the selected group's points change, so that
      -- a field being edited isn't wiped by a render that adds points to
      -- some other group, or adds none.
      syncEntries = readIORef selected >>= mapM_ (\g -> do
        points <- Map.keys <$> getDynamic (agTable g)
        shown <- readIORef shownPoints
        when (Just points /= shown) showEntries)

      addEntry = readIORef selected >>= mapM_ (\g -> do
        txt <- get newPoint text
        case readPoint txt of
          Left err -> say err
          Right pt -> do
            void (addAutocolorEntry st g pt Nothing)
            set newPoint [ text := "" ]
            madeChange
            showEntries)

  set groupChoice [ on select := get groupChoice selection >>= selectGroup ]
  set toleranceEntry [ on command := commitTolerance
                     , on focus := \gained -> unless gained commitTolerance ]
  writeIORef missAction $ \c -> readIORef selected >>= mapM_ (\g -> do
    setAutocolorMissColor st g c
    madeChange)
  set lockBox [ on command := readIORef selected >>= mapM_ (\g -> do
                  b <- get lockBox checked
                  setAutocolorLocked st g b
                  madeChange) ]
  set clearBtn [ on command := readIORef selected >>= mapM_ (\g -> do
                   clearAutocolorGroup st g
                   madeChange
                   showEntries) ]
  set addBtn [ on command := addEntry ]
  set newPoint [ on command := addEntry ]

  set f [ layout := fill $ container p $ margin 5 $ column 5
            [ hfill $ row 5 [ hfill (widget groupChoice), widget lockBox, widget clearBtn ]
            , hfill $ grid 5 5
                [ [ margin 3 (label "Tolerance:"), hfill (widget toleranceEntry) ]
                , [ margin 3 (label "Color for points not in the group:"), hfloatLeft (widget missPicker) ] ]
            , fill (widget list)
            , hfill $ row 5 [ margin 3 (label "New point:"), hfill (widget newPoint), widget addBtn ]
            , hfill (widget status) ]
        , clientSize := sz 520 420 ]

  -- The tables change on render threads; 'wxWatchDynamic' runs these on
  -- the GUI thread. Each group's table is watched once it appears (a
  -- watch can't be stopped, so it lasts as long as the window does).
  watched <- newIORef ([] :: [AutocolorGroupId])
  wxWatchDynamic1 f (autocolorGroupsDynamic st) $ \groups -> do
    already <- readIORef watched
    forM_ (Map.toList groups) $ \(gId, g) -> unless (gId `elem` already) $ do
      modifyIORef' watched (gId :)
      wxWatchDynamic f (agTable g) (\_ -> reportFailure "updating the list" syncEntries)
    reportFailure "updating the groups" syncGroups
  -- (only on the window's own destruction: see 'onOwnDestroy')
  onOwnDestroy f closed
  void (windowShow f)
  windowRaise f
  pure f

-- | Run an action from a watch, reporting (on stderr) anything it throws,
-- rather than letting it vanish into the wx event loop.
reportFailure :: String -> IO () -> IO ()
reportFailure what action = action `catch` \(e :: SomeException) ->
  hPutStrLn stderr ("autocolor window: error " ++ what ++ ": " ++ displayException e)

-- | A color picker showing the given color, running the action with each
-- color picked.
colorPicker :: Window a -> Color -> (Color -> IO ()) -> IO (ColourPickerCtrl ())
colorPicker p c0 picked = do
  let (r0, g0, b0) = colorToRGB c0
  picker <- feed2 [ visible := True ] 0 $
    initialWindow $ \iD rect' ps s -> do
      e <- colorPickerCtrlCreate p iD (rgb r0 g0 b0) rect' s
      set e ps
      pure e
  let newPick = do
        c <- colorPickerCtrlGetColour picker
        picked (rgbToColor (colorRed c, colorGreen c, colorBlue c))
  windowOnEvent picker [wxEVT_COMMAND_COLOURPICKER_CHANGED] newPick (const newPick)
  pure picker
