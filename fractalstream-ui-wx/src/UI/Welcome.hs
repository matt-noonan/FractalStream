-- | The first window FractalStream shows: a browser for the library of
-- projects it comes with ("UI.Library"), plus ways to start a new
-- project or open one from a file. The library's sections are listed
-- down the left, the selected section's projects are shown as a projectGrid
-- of previews, and the selected project's title and description are
-- shown on the right. Double-clicking a project (or choosing it and
-- clicking "Open") opens a copy of it.
module UI.Welcome
  ( welcome
  ) where

import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Event)

import Control.Monad (forM, unless)
import Data.IORef

import FractalStream.Library
import UI.LibraryGrid
import UI.Menu
import UI.PreviewImage (paddedPreviewImage)
import UI.ProjectActions

-- | Show the welcome window, returning it.
welcome :: ProjectActions -> IO (Frame ())
welcome ProjectActions{..} = do

  f <- frame [ text := "FractalStream"
             , on resize := propagateEvent ]

  makeMenuBar ProjectActions{..} f []

  p <- panel f []

  sectionList <- singleListBox p [ items := map lsName librarySections ]

  titleText <- staticText p [ text := "", font := fontDefault { _fontWeight = WeightBold } ]
  descriptionText <- textCtrlEx p (wxTE_MULTILINE .+. wxTE_READONLY .+. wxNO_BORDER) []
  openButton <- button p [ text := "Open", enabled := False ]

  newButton <- button p [ text := "New project"
                        , on command := projectNew ]
  openFileButton <- button p [ text := "Open a file..."
                             , on command := fileOpenDialog f True True
                                 "Open a FractalStream project" projectFileTypes "" ""
                                 >>= mapM_ projectOpen ]

  -- The projects in the section being shown, and which of them (if
  -- any) is selected.
  shown <- newIORef []
  selected <- newIORef Nothing

  let selectEntry mentry = do
        writeIORef selected mentry
        case mentry of
          Nothing -> do
            set titleText [ text := "" ]
            set descriptionText [ text := "Choose a project to see what it's about." ]
            set openButton [ enabled := False ]
          Just LibraryEntry{..} -> do
            set titleText [ text := leTitle ]
            set descriptionText [ text := leDescription ]
            set openButton [ enabled := True ]

      entryAt i = (\es -> if i >= 0 && i < length es then Just (es !! i) else Nothing)
                  <$> readIORef shown

  projectGrid <- libraryGrid p
    (\mi -> maybe (pure Nothing) entryAt mi >>= selectEntry)
    (\i -> entryAt i >>= mapM_ projectOpenLibrary)

  -- Every project's tile is made up front. Previews come in different
  -- shapes, so each is centered on a square of the grid's background
  -- color.
  sectionTiles <- forM librarySections $ \LibrarySection{..} ->
    forM lsEntries $ \LibraryEntry{..} -> do
      img <- paddedPreviewImage gridTileSize (colorSystem ColorWindow) lePreview
      bmp <- bitmapCreateFromImage img (-1)
      pure (leTitle, bmp)

  let showSection sectionIx = case drop sectionIx (zip librarySections sectionTiles) of
        [] -> pure ()
        (LibrarySection{..}, tiles) : _ -> do
          writeIORef shown lsEntries
          setGridItems projectGrid tiles
          -- Start with the section's first project chosen.
          unless (null lsEntries) $ selectGridItem projectGrid (Just 0)

  set sectionList [ on select := get sectionList selection >>= showSection ]
  set openButton [ on command := readIORef selected >>= mapM_ projectOpenLibrary ]

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
            , row 10 [ widget newButton, widget openFileButton, hglue ]
            ]
        ]

  unless (null librarySections) $ do
    set sectionList [ selection := 0 ]
    showSection 0

  windowReLayout f
  pure f
