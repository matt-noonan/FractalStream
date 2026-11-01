-- | A scrolling grid of square tiles, each an image with a title under
-- it, for browsing the project library ("UI.Welcome").
--
-- wx's own list control can show icons in a grid, but not the way we
-- want here (in its generic version, used on macOS and GTK): it fills
-- columns top to bottom and then scrolls sideways, and widens a whole
-- column to fit its longest title rather than wrapping it. So this one
-- is drawn by hand: tiles go left to right, wrapping to the window's
-- width whenever it's resized, rows scroll vertically, and titles wrap
-- onto at most two lines. A tile is selected by clicking it or with the
-- arrow keys, and opened by double-clicking it or with Return.
module UI.LibraryGrid
  ( LibraryGrid
  , libraryGrid
  , gridWindow
  , setGridItems
  , selectGridItem
  , gridTileSize
  ) where

-- (Hiding layout combinators and attributes whose names this module
-- uses for its own grid, rows, and columns.)
import Graphics.UI.WX hiding (grid, row, column, columns, parent, selection, line, next)
import Graphics.UI.WXCore hiding (Event, grid, row, column)

import Control.Monad (forM_)
import Data.IORef

data LibraryGrid = LibraryGrid
  { lgWindow   :: ScrolledWindow ()
  , lgItems    :: IORef [(String, Bitmap ())]
  , lgSelected :: IORef (Maybe Int)
  , lgOnSelect :: Maybe Int -> IO ()
  }

gridWindow :: LibraryGrid -> ScrolledWindow ()
gridWindow = lgWindow

-- | The tiles' size and spacing, in pixels.
tileSize, cellPadding, labelGap, gridMargin, scrollStep :: Int
tileSize    = 128
cellPadding = 12
labelGap    = 6
gridMargin  = 8
scrollStep  = 20

cellWidth :: Int
cellWidth = tileSize + 2 * cellPadding

-- | The size of the (square) images to give the grid for its tiles.
gridTileSize :: Int
gridTileSize = tileSize

-- | Where the tiles go, for the window's current width.
data Geometry = Geometry
  { gColumns    :: Int
  , gLeft       :: Int  -- ^ where the first column starts
  , gCellHeight :: Int
  , gLineHeight :: Int
  }

-- | Make an empty grid. @onSelect@ is told whenever the selection
-- changes (to 'Nothing' when nothing is selected), and @onActivate@
-- when a tile is double-clicked or Return is pressed on it.
libraryGrid :: Window a -> (Maybe Int -> IO ()) -> (Int -> IO ()) -> IO LibraryGrid
libraryGrid parent onSelect onActivate = do
  sw <- scrolledWindow parent [ scrollRate := sz 0 scrollStep
                              , bgcolor := colorSystem ColorWindow
                              , fullRepaintOnResize := False ]
  tiles <- newIORef []
  selection <- newIORef Nothing
  let grid = LibraryGrid sw tiles selection onSelect

      -- (wxHaskell already gives a mouse event's position in the grid's
      -- own coordinates, scrolling included.)
      itemAt (Point x y) = do
        g <- geometry grid

        n <- length <$> readIORef tiles
        let col = (x - gLeft g) `div` cellWidth
            row = (y - gridMargin) `div` gCellHeight g
            i = row * gColumns g + col
        pure $ if x >= gLeft g && y >= gridMargin && col < gColumns g && i < n
                 then Just i else Nothing

      moveBy delta = do
        n <- length <$> readIORef tiles
        current <- readIORef selection
        let target = maybe 0 (+ delta) current
        when (n > 0 && target >= 0 && target < n) $ selectGridItem grid (Just target)

  set sw
    [ on paint := paintGrid grid
    , on resize := relayout grid >> propagateEvent
    , on click := \pos -> do
        windowSetFocus sw
        itemAt pos >>= selectGridItem grid
    , on doubleClick := \pos -> itemAt pos >>= mapM_ onActivate
    , on keyboard := \k -> do
        g <- geometry grid
        case keyKey k of
          KeyLeft   -> moveBy (-1)
          KeyRight  -> moveBy 1
          KeyUp     -> moveBy (negate (gColumns g))
          KeyDown   -> moveBy (gColumns g)
          KeyReturn -> readIORef selection >>= mapM_ onActivate
          _         -> propagateEvent
    ]
  pure grid

-- | Show these tiles instead of whatever was there, scrolled to the
-- top, with nothing selected.
setGridItems :: LibraryGrid -> [(String, Bitmap ())] -> IO ()
setGridItems grid tiles = do
  writeIORef (lgItems grid) tiles
  writeIORef (lgSelected grid) Nothing
  lgOnSelect grid Nothing
  scrolledWindowScroll (lgWindow grid) (pt 0 0)
  relayout grid

-- | Select a tile (or none), scrolling it into view.
selectGridItem :: LibraryGrid -> Maybe Int -> IO ()
selectGridItem grid i = do
  old <- readIORef (lgSelected grid)
  when (i /= old) $ do
    writeIORef (lgSelected grid) i
    forM_ i (scrollIntoView grid)
    repaint (lgWindow grid)
    lgOnSelect grid i

geometry :: LibraryGrid -> IO Geometry
geometry grid = do
  let sw = lgWindow grid
  Size width _ <- get sw clientSize
  Size _ lineHeight <- withClientDC sw (\dc -> getTextExtent dc "Ag")
  let columns = max 1 ((width - 2 * gridMargin) `div` cellWidth)
  pure Geometry
    { gColumns = columns
    , gLeft = max gridMargin ((width - columns * cellWidth) `div` 2)
    , gCellHeight = cellPadding + tileSize + labelGap + 2 * lineHeight + cellPadding
    , gLineHeight = lineHeight
    }

cellOrigin :: Geometry -> Int -> Point
cellOrigin g i = pt (gLeft g + (i `mod` gColumns g) * cellWidth)
                    (gridMargin + (i `div` gColumns g) * gCellHeight g)

-- | Work out the scrollable area again, for the window's current width.
relayout :: LibraryGrid -> IO ()
relayout grid = do
  g <- geometry grid
  n <- length <$> readIORef (lgItems grid)
  Size width _ <- get (lgWindow grid) clientSize
  let rows = (n + gColumns g - 1) `div` gColumns g
  windowSetVirtualSize (lgWindow grid) (sz width (2 * gridMargin + rows * gCellHeight g))
  repaint (lgWindow grid)

scrollIntoView :: LibraryGrid -> Int -> IO ()
scrollIntoView grid i = do
  let sw = lgWindow grid
  g <- geometry grid
  Point _ viewTop <- scrolledWindowCalcUnscrolledPosition sw (pt 0 0)
  Size _ viewHeight <- get sw clientSize
  let Point _ top = cellOrigin g i
      bottom = top + gCellHeight g
  when (top < viewTop) $
    scrolledWindowScroll sw (pt 0 (top `div` scrollStep))
  when (bottom > viewTop + viewHeight) $
    scrolledWindowScroll sw (pt 0 ((bottom - viewHeight + scrollStep - 1) `div` scrollStep))

paintGrid :: LibraryGrid -> DC a -> Rect -> IO ()
paintGrid grid dc _ = do
  g <- geometry grid
  shownTiles <- readIORef (lgItems grid)
  current <- readIORef (lgSelected grid)
  forM_ (zip [0 ..] shownTiles) $ \(i, (title, bmp)) -> do
    let Point x y = cellOrigin g i
        isSelected = current == Just i
        labelColor = colorSystem (if isSelected then ColorHighlightText else ColorWindowText)
    when isSelected $
      drawRect dc (rect (pt x y) (sz cellWidth (gCellHeight g)))
        [ brushKind := BrushSolid, brushColor := colorSystem ColorHighlight
        , penKind := PenTransparent ]
    drawBitmap dc bmp (pt (x + cellPadding) (y + cellPadding)) True []
    labelLines <- wrapLabel dc (cellWidth - 8) title
    forM_ (zip [0 ..] labelLines) $ \(row, line) -> do
      Size w _ <- getTextExtent dc line
      drawText dc line
        (pt (x + (cellWidth - w) `div` 2)
            (y + cellPadding + tileSize + labelGap + row * gLineHeight g))
        [ textColor := labelColor ]

-- | Break a title into at most two lines no wider than @maxWidth@, at
-- spaces, ending the second line with "…" if it doesn't all fit.
wrapLabel :: DC a -> Int -> String -> IO [String]
wrapLabel dc maxWidth title = do
  first <- fillLine (words title)
  case first of
    (line1, []) -> pure [line1]
    (line1, rest) -> do
      (line2, leftover) <- fillLine rest
      if null leftover then pure [line1, line2]
                       else (\l -> [line1, l]) <$> ellipsize (unwords rest)
  where
    textWidth s = (\(Size w _) -> w) <$> getTextExtent dc s
    -- As many words as fit on one line (always at least one, which
    -- 'ellipsize' shortens if it's too long by itself).
    fillLine [] = pure ("", [])
    fillLine (w : ws) = go w ws
      where
        go line (next : more) = do
          let candidate = line ++ " " ++ next
          fits <- (<= maxWidth) <$> textWidth candidate
          if fits then go candidate more else finish line (next : more)
        go line [] = finish line []
        finish line more = do
          fits <- (<= maxWidth) <$> textWidth line
          if fits then pure (line, more) else (\l -> (l, more)) <$> ellipsize line
    ellipsize s = do
      fits <- (<= maxWidth) <$> textWidth s
      if fits || null s then pure s else shorten (init s)
    shorten s = do
      let candidate = s ++ "…"
      fits <- (<= maxWidth) <$> textWidth candidate
      if fits || null s then pure candidate else shorten (init s)
