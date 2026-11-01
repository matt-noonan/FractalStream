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

import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Event)

import Control.Monad (forM, unless)
import Data.Functor ((<&>))
import Data.IORef
import Data.Maybe (fromMaybe, isJust, isNothing)
import System.Directory (createDirectoryIfMissing)

import FractalStream.Library
import UI.LibraryGrid
import UI.Menu
import UI.PreviewImage (paddedPreviewImage)
import UI.ShowInFileManager (showInFileManager)
import UI.ProjectActions

-- | Show the welcome window. Returns it, along with an action that
-- switches it to the "Starting points" section (for "New project").
welcome :: ProjectActions -> IO (Frame (), IO ())
welcome ProjectActions{..} = do

  f <- frame [ text := "FractalStream"
             , on resize := propagateEvent ]

  makeMenuBar ProjectActions{..} f []

  p <- panel f []

  sectionList <- singleListBox p []

  titleText <- staticText p [ text := "", font := fontDefault { _fontWeight = WeightBold } ]
  descriptionText <- textCtrlEx p (wxTE_MULTILINE .+. wxTE_READONLY .+. wxNO_BORDER) []
  openButton <- button p [ text := "Open", enabled := False ]

  newButton <- button p [ text := "New project"
                        , on command := projectNew ]
  openFileButton <- button p [ text := "Open a file..."
                             , on command := fileOpenDialog f True True
                                 "Open a FractalStream project" projectFileTypes "" ""
                                 >>= mapM_ projectOpen ]
  showFolderButton <- button p [ text := "Show folder", enabled := False ]

  myProjectsDir <- myProjectsDirectory

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
            set descriptionText [ font := fontDefault ]

            inMine <- readIORef showingMine
            set descriptionText [ text := if inMine then emptyMine else
                                    "Choose a project to see what it's about." ]
            set openButton [ enabled := False ]
          Just LibraryEntry{..} -> do
            set titleText [ text := leTitle ]
            -- A problem's script error marks where it is with a line of
            -- ^^^ under the script, which only lines up in a fixed-width
            -- font.
            set descriptionText [ font := if isJust leProblem then fontFixed else fontDefault
                                , text := fromMaybe leDescription leProblem ]
            set openButton [ enabled := isNothing leProblem ]

      emptyMine = "Your own projects go in " ++ myProjectsDir ++ ". Save a project \
                  \there (or in a folder inside it, to give it a section of its \
                  \own) to see it here."

      entryAt i = (\es -> if i >= 0 && i < length es then Just (es !! i) else Nothing)
                  <$> readIORef shown

  projectGrid <- libraryGrid p
    (\mi -> maybe (pure Nothing) entryAt mi >>= selectEntry)
    (\i -> entryAt i >>= mapM_ (\e -> unless (isJust (leProblem e)) (projectOpenLibrary e)))

  -- Every built-in project's tile is made up front. Previews come in
  -- different shapes, so each is centered on a square of the grid's
  -- background color.
  builtIn <- forM librarySections $ \s -> (s,) <$> tilesFor (lsEntries s)
  let builtInCount = length builtIn
      isMine i = i >= builtInCount

      readMine = do
        sections <- scanProjectsIn myProjectsDir
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
  set openButton [ on command := readIORef selected >>= mapM_ projectOpenLibrary ]
  set showFolderButton [ on command := do
                           createDirectoryIfMissing True myProjectsDir
                           showInFileManager myProjectsDir ]
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

  -- The section list keeps its width, the grid takes up all the extra
  -- room, and the details keep their width while the description gets
  -- all of their extra height. (See the notes on wxHaskell's layout
  -- rules in CLAUDE_NOTES.md.)
  set f [ layout := container p $ margin 10 $ column 10
            [ row 10
                [ vfill (minsize (sz 170 360) (widget sectionList))
                , fill (minsize (sz 480 360) (widget (gridWindow projectGrid)))
                , column 5
                    [ widget titleText
                    , fill (minsize (sz 260 300) (widget descriptionText))
                    , row 5 [ hglue, widget openButton ]
                    ]
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
  pure (f, mapM_ selectSection (take 1 (sectionIndices lsStartingPoints)))

