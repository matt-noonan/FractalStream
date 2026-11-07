-- | The first window FractalStream shows: a browser for the library of
-- projects it comes with ("FractalStream.Library"), and for the user's
-- own projects folder, plus ways to start a new project or open one from
-- a file. The sections are listed down the left (the built-in ones, then
-- "My projects" and its subfolders), the selected section's projects are
-- shown as a grid of previews ("UI.LibraryGrid"), and the selected
-- project's title and description are shown on the right.
-- Double-clicking a project (or choosing it and clicking "Open") opens a
-- copy of a built-in one, or the user's own project itself.
module UI.Welcome
  ( welcome
  ) where

import FractalStream.Quoted (quoted)
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Event)

import Control.Monad (forM, unless)
import Data.Functor ((<&>))
import Data.IORef
import Data.Maybe (fromMaybe, isJust)
import System.Directory (createDirectoryIfMissing)

import FractalStream.Library
import UI.LibraryGrid
import UI.Menu
import UI.PreviewImage (paddedPreviewImage)
import UI.ShowInFileManager (showInFileManager)
import UI.ProjectActions

-- | The sash width to ask for between the projects and their details.
-- (wxHaskell's layout does not pass this on, so wxWidgets picks the width;
-- the resize handler below asks the splitter for the one it settled on.)
sashWidth :: Int
sashWidth = 6

-- | How the two panes' room is shared when the window opens: the projects
-- get the width they used to be fixed at, and the details get the rest of
-- what the window has always needed for the two of them.
projectsStartWidth, detailsStartWidth, panesStartWidth :: Int
projectsStartWidth = 480
detailsStartWidth  = 264
panesStartWidth    = projectsStartWidth + sashWidth + detailsStartWidth

-- | The narrowest the projects and the details may be made: the grid
-- reflows, so two columns of tiles are enough for it, and the description
-- gets no narrower than it has always been.
projectsMinWidth, detailsMinWidth :: Int
projectsMinWidth = 320
detailsMinWidth  = 260

-- | Show the welcome window. Returns it, along with an action that
-- switches it to the "Starting points" section (for "New project").
welcome :: ProjectActions -> IO (Frame (), IO ())
welcome ProjectActions{..} = do

  f <- frame [ text := "FractalStream"
             , on resize := propagateEvent ]

  makeMenuBar ProjectActions{..} Nothing f []

  p <- panel f []

  -- The sections go down the left, and the projects and their details
  -- share the rest of the window behind a sash (see the layout at the
  -- end), so that a description can be given as much room as it needs.
  splitter <- splitterWindow p []
  gridPanel <- panel splitter []
  detailsPanel <- panel splitter []

  sectionList <- singleListBox p []

  titleText <- staticText detailsPanel
                 [ text := ""
                 , font := fontDefault { _fontWeight = WeightBold, _fontSize = 14 } ]
  descriptionText <- textCtrlEx detailsPanel
                       (wxTE_MULTILINE .+. wxTE_READONLY .+. wxNO_BORDER) []

  newButton <- button p [ text := "New project"
                        , on command := projectNew ]
  openFileButton <- button p [ text := "Open a file..."
                             , on command := fileOpenDialog f True True
                                 "Open a FractalStream project" projectFileTypes "" ""
                                 >>= mapM_ projectOpen ]
  showFolderButton <- button p [ text := "Show folder", enabled := False ]


  -- The projects in the section being shown, which of them (if any) is
  -- selected, and the user's own sections: not read until one of them
  -- is first wanted (reading the documents folder can make macOS ask
  -- the user's permission, which should happen when it makes sense),
  -- and read again whenever the window comes back to the front.
  shown <- newIORef []
  selected <- newIORef Nothing
  mine <- newIORef (Nothing :: Maybe [(LibrarySection, [(String, Bitmap ())])])
  showingMine <- newIORef False

  let tilesFor entries = forM entries $ \LibraryEntry{..} -> do
        img <- paddedPreviewImage gridTileSize (colorSystem ColorWindow)
                 (Just (fromMaybe placeholderPreview lePreview))
        bmp <- bitmapCreateFromImage img (-1)
        pure (leTitle, bmp)

      selectEntry mentry = do
        writeIORef selected mentry
        case mentry of
          Nothing -> do
            set titleText [ text := "" ]
            set descriptionText [ font := fontDefault { _fontSize = 14 } ]

            inMine <- readIORef showingMine
            folder <- projectsFolder
            set descriptionText [ text := if inMine then emptyMine folder else
                                    "Choose a project to see what it's about." ]
          Just LibraryEntry{..} -> do
            set titleText [ text := leTitle ]
            -- A problem's script error marks where it is with a line of
            -- ^^^ under the script, which only lines up in a fixed-width
            -- font.
            set descriptionText [ font := (if isJust leProblem then fontFixed else fontDefault)
                                  { _fontSize = 14 }
                                , text := coalesceNewlines $
                                          fromMaybe leDescription leProblem ]

      emptyMine folder = "Your own projects go in " ++ quoted folder ++ ". Save a project \
                  \there (or in a folder inside it, to give it a section of its \
                  \own) to see it here."

      entryAt i = (\es -> if i >= 0 && i < length es then Just (es !! i) else Nothing)
                  <$> readIORef shown

  projectGrid <- libraryGrid gridPanel
    (\mi -> maybe (pure Nothing) entryAt mi >>= selectEntry)
    (\i -> entryAt i >>= mapM_ (\e -> when (leCanOpen e) (projectOpenLibrary e)))


  -- Every built-in project's tile is made up front. Previews come in
  -- different shapes, so each is centered on a square of the grid's
  -- background color.
  builtIn <- forM librarySections $ \s -> (s,) <$> tilesFor (lsEntries s)
  let builtInCount = length builtIn
      isMine i = i >= builtInCount

      readMine = do
        sections <- scanProjectsIn =<< projectsFolder
        scanned <- forM sections $ \s -> (s,) <$> tilesFor (lsEntries s)
        writeIORef mine (Just scanned)
        pure scanned

      -- The sidebar: the built-in sections, then the user's own, with
      -- their subfolders indented under "My projects".
      sidebarItems = readIORef mine <&> \m ->
        map (lsName . fst) builtIn ++ case m of
          Nothing -> ["My projects"]
          Just scanned -> [ (if i == 0 then "" else "    ") ++ lsName s
                          | (i, (s, _)) <- zip [0 :: Int ..] scanned ]

      showSection i = do
        sections <- if isMine i
          then (builtIn ++) <$> (readIORef mine >>= maybe (refreshMine >> readMineSections) pure)
          else pure builtIn
        set showFolderButton [ enabled := isMine i ]
        writeIORef showingMine (isMine i)
        case drop i sections of
          [] -> pure ()
          (LibrarySection{..}, tiles) : _ -> do
            writeIORef shown lsEntries
            setGridItems projectGrid tiles
            -- Start with the section's first project chosen.
            unless (null lsEntries) $ selectGridItem projectGrid (Just 0)

      readMineSections = fromMaybe [] <$> readIORef mine

      -- Read the user's sections again, keeping whatever's selected.
      refreshMine = do
        current <- get sectionList selection
        _ <- readMine
        newItems <- sidebarItems
        set sectionList [ items := newItems
                        , selection := min current (length newItems - 1) ]

  set sectionList [ on select := get sectionList selection >>= showSection ]
  set showFolderButton [ on command := do
                           folder <- projectsFolder
                           createDirectoryIfMissing True folder
                           showInFileManager folder ]

  set f [ on activate :~ \previous active -> do
            -- Back in front: pick up any changes made to the user's
            -- projects folder meanwhile (if it's been read at all yet).
            scanned <- isJust <$> readIORef mine
            when (active && scanned) $ do
              refreshMine
              current <- get sectionList selection
              when (isMine current) (showSection current)
            previous active ]

  sidebarItems >>= \initial -> set sectionList [ items := initial ]

  -- How the two panes share the width: until the sash is dragged they keep
  -- their shares, so enlarging the window widens the description as well;
  -- after a drag the details keep the width they were given, and the grid
  -- takes up whatever the window's size leaves over. (The panes are laid out
  -- again after this handler runs, so the width to keep is the details' width
  -- from before the resize.)
  chosenDetailsWidth <- newIORef (Nothing :: Maybe Int)
  lastSash <- newIORef (Nothing :: Maybe Int)
  set splitter [ on resize := do
                   whole <- get splitter clientSize
                   details <- get detailsPanel clientSize
                   sash <- splitterWindowGetSashSize splitter
                   current <- splitterWindowGetSashPosition splitter
                   previous <- readIORef lastSash
                   -- (A sash that is not where we left it has been dragged:
                   -- from then on the details keep the width they have.
                   -- Before the first layout they have no width of their own,
                   -- which their minimum rules out.)
                   case previous of
                     Nothing -> pure ()
                     Just was -> when (was /= current && sizeW details >= detailsMinWidth) $
                                   writeIORef chosenDetailsWidth (Just (sizeW details))
                   chosen <- readIORef chosenDetailsWidth
                   -- (The sash never goes past either pane's minimum.)
                   let shared = sizeW whole - sash
                       keep = case chosen of
                         Just width -> shared - width
                         Nothing    -> (shared * projectsStartWidth) `div` panesStartWidth
                       wanted = max projectsMinWidth (min (shared - detailsMinWidth) keep)
                   when (wanted /= current) $
                     splitterWindowSetSashPosition splitter wanted False
                   -- (Where it really ended up: wxWidgets may have clamped it.)
                   splitterWindowGetSashPosition splitter >>= \settled ->
                     writeIORef lastSash (Just settled)
                   propagateEvent ]

  -- The section list keeps its width, the projects and the details share
  -- the rest, and the details keep their height while the description
  -- gets all of theirs. The sash between the two sets how the width is
  -- shared: it starts where the projects' width used to be fixed, and the
  -- details get the rest, as they did before. Each side has a minimum it
  -- can't be dragged past, and the two together take the room the window
  -- has always needed. (See the notes on wxHaskell's layout rules in
  -- CLAUDE_NOTES.md.)
  set f [ layout := container p $ margin 10 $ column 10
            [ row 10
                [ vfill (minsize (sz 170 360) (widget sectionList))
                , fill (minsize (sz panesStartWidth 360) $
                        vsplit splitter sashWidth projectsStartWidth
                          (container gridPanel
                             (fill (minsize (sz projectsMinWidth 360)
                                            (widget (gridWindow projectGrid)))))
                          (container detailsPanel
                             (column 5
                                -- (The title stretches so that its column,
                                -- and the description in it, take the width
                                -- the sash gives them.)
                                [ hfill (widget titleText)
                                , fill (minsize (sz detailsMinWidth 300)
                                                (widget descriptionText))
                                ])))
                ]
            , row 10 [ widget newButton, widget openFileButton, hglue, widget showFolderButton ]
            ]
        ]

  let selectSection i = do
        set sectionList [ selection := i ]
        showSection i
      sectionIndices which = [ i | (i, (s, _)) <- zip [0 ..] builtIn, which s ]

  -- Open on the first section of finished projects to explore; "New
  -- project" switches to the starting points.
  case sectionIndices (not . lsStartingPoints) ++ sectionIndices (const True) of
    i : _ -> selectSection i
    [] -> pure ()

  windowReLayout f
  -- On GTK, a frame's menu bar only has a height once the frame is on
  -- screen, so the layout above leaves the window (and the smallest it
  -- can be made) short by the menu bar's height, hiding the bottom row
  -- of buttons. Laying it out again once it's showing puts that right.
  relayout <- timer f [ interval := 100 ]
  set relayout [ on command := do
                   set relayout [ enabled := False ]
                   windowReLayout f ]
  pure (f, mapM_ selectSection (take 1 (sectionIndices lsStartingPoints)))

-- | Replace single newlines with spaces, coalesce multiple newlines into one.
coalesceNewlines :: String -> String
coalesceNewlines s = case s of
  ('\n' : etc@('\n' : '\n' : _)) -> coalesceNewlines etc
  ('\n' : '\n' : etc) -> '\n' : coalesceNewlines etc
  ('\n' : etc) -> ' ' : coalesceNewlines etc
  (c : etc) -> c : coalesceNewlines etc
  "" -> ""
