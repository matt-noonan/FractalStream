{- |
Module      : UI.MovieMaker
Description : Making a movie of a viewer, from the viewer's menu

The wx side of "Actor.Movie": a dialog asking how to make the movie, and
a window showing its progress, which becomes a window offering to save
it once it's done. Movies are recorded to a temporary file and opened in
the system's own movie player, from which they can be watched, and saved
or not; the window here can also save a copy.

There are two ways to ask for a movie ('MovieSource'):

* "Capture > Movie..." in a viewer's menu: a script with a
  @define animate(t):@ block, run each frame with the time going from 0
  to 1, that sets the project's values;
* the "Animate along path" tool: a path dragged out in a viewer whose
  coordinate is a configuration variable (a parameter plane, say), which
  moves that variable along it.

Either way, any viewer of the project can be recorded, so the project's
viewers are kept in a 'MovieSupport' as their windows come and go. It
also remembers the last movie's settings, so that the next one can
start from them.
-}
module UI.MovieMaker
  ( MovieSupport
  , newMovieSupport
  , registerMovieViewer
  , MovieSource(..)
  , MovieProject(..)
  , makeMovieDialog
  ) where

import FractalStream.Prelude hiding (get)

import Graphics.UI.WX hiding (finally, when, Object, Dimensions, Layout, Color, Horizontal, Vertical, pt, tool)
import qualified Graphics.UI.WXCore.Events as WX
import Graphics.UI.WXCore.WxcClassesMZ (windowDestroy, windowShow, windowRaise)
import Graphics.UI.WXCore.WxcDefs (wxCAPTION, wxCLOSE_BOX, wxSYSTEM_MENU)

import Actor.Event (EventArgument_)
import Actor.Layout (Dimensions(..), Field(..))
import Actor.Movie
import Actor.Viewer (Viewer(..), cloneViewer, describeViewerIssues)
import Data.DynamicValue
import Data.Movie
import FractalStream.Quoted (quoted)
import Language.Environment (SomeContext(..))
import Language.Code.Parser (CommonDefs, noCommonDefs)
import Actor.Layout (CodeString(..))
import Graphics.UI.WXCore.WxcClasses (styledTextCtrlGetText)

import UI.ShowInFileManager (openWithDefaultApp)
import UI.Widgets (wxTimer, onOwnDestroy, scriptWidget)

import Control.Concurrent.Async (async, cancel, poll, AsyncCancelled(..))
import Control.Exception (SomeException, displayException, try, finally, fromException)
import Data.IORef
import Data.Time (getCurrentTime, diffUTCTime, getZonedTime, formatTime, defaultTimeLocale)
import Data.Unique (Unique, newUnique)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, getHomeDirectory,
                         doesDirectoryExist, copyFile, getFileSize)
import System.FilePath ((</>), (<.>), takeFileName, takeDirectory)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | What a project keeps for making movies: its viewers, and the last
-- movie's settings.
data MovieSupport = MovieSupport
  { msViewers :: IORef [(Unique, Viewer)]
  , msLast    :: IORef (Maybe LastMovie)
  }

-- | What was asked for last time, to start the next dialog from.
data LastMovie = LastMovie
  { lmViewer   :: String
  , lmScript   :: Maybe String
  , lmSeconds  :: String
  , lmFps      :: String
  , lmScale    :: String
  , lmSmooth   :: Bool
  , lmDrawings :: Bool
  , lmLoop     :: Bool
  }

newMovieSupport :: IO MovieSupport
newMovieSupport = MovieSupport <$> newIORef [] <*> newIORef Nothing

-- | Make a viewer available for movies; returns the action that takes
-- it away again (when its window goes).
registerMovieViewer :: MovieSupport -> Viewer -> IO (IO ())
registerMovieViewer MovieSupport{..} viewer = do
  ident <- newUnique
  modifyIORef' msViewers (++ [(ident, viewer)])
  pure (modifyIORef' msViewers (filter ((/= ident) . fst)))

-- | What a movie animates.
data MovieSource
  = ScriptSource
    -- ^ a script that sets the project's values, with @t@ from 0 to 1
  | PathSource String String [(Double, Double)]
    -- ^ a path, drawn in the viewer with this title, for its coordinate
    -- (a complex configuration variable, of this name) to move along

-- | What the movie dialog needs to know about the project.
data MovieProject = MovieProject
  { mpSupport     :: MovieSupport
  , mpConfig      :: SomeContext EventArgument_
    -- ^ the project's configuration variables
  , mpCommonDefs  :: IO (Either String CommonDefs)
    -- ^ the definitions the project's @\@common@ stanzas make, which a
    -- movie's script can use
  , mpFields      :: IO [Field]
    -- ^ the fields of the project's configuration
  , mpUnsaved     :: Variable Bool
    -- ^ whether the project has unsaved changes: recording a movie
    -- changes values, but puts them back afterwards, so it doesn't count
  , mpCoordinate  :: String -> IO (Maybe String)
    -- ^ the coordinate variable of the viewer with this title
  , mpProjectName :: IO String
  , mpOwner       :: Window ()
    -- ^ the project's own (hidden) window, which the progress window
    -- belongs to: it lives as long as the project does
  }

-- | A movie, ready to record.
data MovieJob = MovieJob
  { mjViewer    :: Viewer
  , mjTitle     :: String
  , mjSettings  :: MovieSettings
  , mjAnimation :: Animation
  }

-- | Ask how to make a movie, from the viewer with the given title (the
-- one whose menu or tool asked), and then make it.
makeMovieDialog :: Window a -> MovieProject -> String -> MovieSource -> IO ()
makeMovieDialog owner project@MovieProject{..} fromTitle source = do
  viewers <- map snd <$> readIORef (msViewers mpSupport)
  titles <- mapM (getDynamic . vTitle) viewers
  lastMovie <- readIORef (msLast mpSupport)
  let complexVars = complexVariables mpConfig
  do
      -- Which viewer to record: the last one recorded, if it's still
      -- here; else, for a path, another viewer than the one it was
      -- drawn in (a path in a parameter plane is for watching the
      -- dynamical plane change), and for a script, this one.
      let others = filter (/= fromTitle) titles
          startTitle = case (lastMovie, source) of
            (Just LastMovie{lmViewer}, _) | lmViewer `elem` titles -> lmViewer
            (_, PathSource{}) | t : _ <- others -> t
            _ -> fromTitle
          startIndex = fromMaybe 0 (lookup startTitle (zip titles [0 ..]))

      d <- dialog owner [ text := "Make a movie" ]
      p <- panel d []
      viewerChoice <- choice p [ items := titles, selection := startIndex ]

      (animationLayout, getAnimation, chosenScript) <- case source of
        ScriptSource -> do
          initial <- case lastMovie >>= lmScript of
            Just s -> pure s
            Nothing -> exampleScript mpFields complexVars
          -- The same editor as the project's script, checking the
          -- script as it's typed (just as recording will) and showing
          -- the first problem in place.
          defsOrError <- mpCommonDefs
          let defs = fromRight noCommonDefs defsOrError
          scriptSource <- newVariable (CodeString initial)
          scriptCheck <- newMapped
            (pure (\(CodeString s) -> checkMovieScript defs mpConfig s)) scriptSource
          (editor, problem) <- scriptWidget p scriptCheck
          let getScript = styledTextCtrlGetText editor
          pure ( column 5
                   [ hfill (label "Each frame, run this script, with t going from 0 (the first frame) to 1 (the last):")
                   , fill (minsize (sz 460 110) (widget editor))
                   , hfill (widget problem) ]
               , \_ -> case defsOrError of
                   Left err -> pure (Left err)
                   Right _ -> getScript >>= scriptAnimation defs mpConfig
               , Just <$> getScript )
        PathSource drawnIn var path ->
          pure ( hfill (label (var ++ " will move along the path drawn in " ++ quoted drawnIn ++ "."))
               , \backToStart -> pure (pathAnimation mpConfig var backToStart path)
               , pure Nothing )

      let lastOr :: forall x. (LastMovie -> x) -> x -> x
          lastOr f def = maybe def f lastMovie
      secondsBox <- textEntry p [ text := lastOr lmSeconds "5" ]
      fpsBox     <- textEntry p [ text := lastOr lmFps "30" ]
      scaleBox   <- textEntry p [ text := lastOr lmScale "1" ]
      sizeNote   <- staticText p [ text := "" ]
      smoothBox  <- checkBox p [ text := "Use subpixel smoothing"
                               , checked := lastOr lmSmooth True ]
      drawingsBox <- checkBox p [ text := "Include what the viewer's tools have drawn"
                                , checked := lastOr lmDrawings False ]
      let loopTip = case source of
            ScriptSource -> "Leave out the last frame (t = 1), since a movie playing in a loop goes back to the first next."
            PathSource{} -> "Come back to the start along a straight line, and leave out the last frame, since a movie playing in a loop goes back to the first next."
      loopBox    <- checkBox p [ text := "Loop back to start"
                               , tooltip := loopTip
                               , checked := lastOr lmLoop (isClosedPath source) ]
      ok     <- button p [ text := "Record" ]
      cancelButton <- button p [ text := "Cancel" ]

      -- The size of the frames, which depends on the viewer and the
      -- multiplier.
      let updateSizeNote = do
            ix <- get viewerChoice selection
            mult <- readMaybe <$> get scaleBox text
            note <- case (drop ix viewers, mult :: Maybe Int) of
              (v : _, Just m) | m > 0 -> do
                Dimensions (w, h) <- getDynamic (vSize v)
                pure (printf "× the viewer's size: %d × %d pixels" (w * m) (h * m))
              _ -> pure "× the viewer's size"
            set sizeNote [ text := note ]
      updateSizeNote
      set viewerChoice [ on select := updateSizeNote ]
      set scaleBox [ on update := updateSizeNote ]

      set d [ layout := margin 10 $ fill $ container p $ column 8
              [ row 5 [ margin 3 (label "Record the viewer"), minsize (sz 220 (-1)) (widget viewerChoice), hglue ]
              , animationLayout
              , row 5 [ margin 3 (label "Length:"), minsize (sz 60 (-1)) (widget secondsBox)
                      , margin 3 (label "seconds, at"), minsize (sz 50 (-1)) (widget fpsBox)
                      , margin 3 (label "frames a second"), hglue ]
              , row 5 [ margin 3 (label "Size:"), minsize (sz 50 (-1)) (widget scaleBox)
                      , margin 3 (hfill (widget sizeNote)) ]
              , widget smoothBox
              , widget drawingsBox
              , widget loopBox
              , row 5 [ margin 5 (widget cancelButton), hglue, margin 5 (widget ok) ] ] ]

      result <- showModal d $ \done -> do
        set cancelButton [ on command := done Nothing ]
        set ok [ on command := do
          ix <- get viewerChoice selection
          seconds <- get secondsBox text
          fps <- get fpsBox text
          scale <- get scaleBox text
          smooth <- get smoothBox checked
          drawings <- get drawingsBox checked
          loop <- get loopBox checked
          script <- chosenScript
          writeIORef (msLast mpSupport) . Just $ LastMovie
            { lmViewer = fromMaybe "" (listToMaybe (drop ix titles)), lmScript = script
            , lmSeconds = seconds, lmFps = fps, lmScale = scale
            , lmSmooth = smooth, lmDrawings = drawings, lmLoop = loop }
          let problem = errorDialog d "Can't make this movie"
          case ( readNumber seconds :: Maybe Double, readNumber fps :: Maybe Int
               , readNumber scale :: Maybe Int, drop ix viewers ) of
            (Nothing, _, _, _) -> problem "The length should be a number of seconds, more than zero."
            (_, Nothing, _, _) -> problem "The frame rate should be a whole number of frames a second, more than zero."
            (_, _, Nothing, _) -> problem "The size should be a whole number, 1 or more: the frames' size, as a multiple of the viewer's."
            (_, _, _, []) -> problem "There's no viewer to record."
            (Just secs, Just rate, Just mult, viewer : _) -> getAnimation loop >>= \case
              Left err -> problem err
              Right animation -> do
                scaled <- cloneViewer viewer
                modifyValue (vSize scaled) (\(Dimensions (w, h)) -> Dimensions (w * mult, h * mult))
                modifyValue (vPixelSize scaled) (/ fromIntegral mult)
                done $ Just MovieJob
                  { mjViewer = scaled
                  , mjTitle = titles !! ix
                  , mjSettings = defaultMovieSettings
                      { msFrames = max 1 (round (secs * fromIntegral rate))
                      , msFps = rate, msLoop = loop, msSmooth = smooth
                      , msWithDrawings = drawings }
                  , mjAnimation = animation } ]

      forM_ result (recordWithProgress project)

-- | A number, more than zero.
readNumber :: (Read a, Ord a, Num a) => String -> Maybe a
readNumber s = case readMaybe s of
  Just x | x > 0 -> Just x
  _ -> Nothing

-- | Whether a path ends where it started (or near enough), so that a
-- movie along it would play well in a loop.
isClosedPath :: MovieSource -> Bool
isClosedPath = \case
  ScriptSource -> False
  PathSource _ _ path@(firstPoint : _ : _) ->
    let end = last path
        dist (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int))
        total = sum (zipWith dist path (drop 1 path))
    in dist firstPoint end < 0.05 * total
  PathSource{} -> False

-- | A script to start with: moving the first complex variable around a
-- small circle, from where it is now; or else nothing.
exampleScript :: IO [Field] -> [String] -> IO String
exampleScript getFields = \case
  [] -> pure (movieScriptTemplate ["pass"])
  var : _ -> do
    fields <- getFields
    current <- case filter ((== var) . fieldName) fields of
      f : _ -> fieldText f
      [] -> pure var
    pure (movieScriptTemplate [var ++ " <- (" ++ current ++ ") + 0.05 e^(2 pi i t)"])

---------------------------------------------------------------------------
-- Recording
---------------------------------------------------------------------------

-- | Record a movie, showing a window with its progress, from which it
-- can be cancelled. (It's an ordinary window, not a tool window: macOS
-- hides those while another app is in front, and the movie player
-- comes to the front once the movie is done.) Once it's done, the movie is opened in the
-- system's movie player, and the window offers to save a copy.
recordWithProgress :: MovieProject -> MovieJob -> IO ()
recordWithProgress MovieProject{..} MovieJob{..} = do
  projectName <- mpProjectName
  path <- movieFileName projectName mjTitle (msFormat mjSettings)
  fields <- mpFields
  wasUnsaved <- getDynamic mpUnsaved
  progress <- newIORef (0 :: Int)
  started <- getCurrentTime

  let total = msFrames mjSettings
      -- The project's values go back to how they were afterwards, and so
      -- does whether it has unsaved changes.
      record = withFieldsRestored fields
                 (recordMovie mjSettings mjViewer mjAnimation (writeIORef progress) path)
               `finally` unless wasUnsaved (setValue' mpUnsaved False)
  worker <- async record

  f <- frameEx (wxCAPTION .+. wxCLOSE_BOX .+. wxSYSTEM_MENU)
               [ text := "Recording a movie" ] mpOwner
  p <- panel f []
  status <- staticText p [ text := "Recording " ++ quoted mjTitle ++ "..." ]
  detail <- staticText p [ text := printf "Frame 0 of %d" total ]
  gauge <- hgauge p (max 1 total) [ selection := 0 ]
  cancelButton <- button p [ text := "Cancel" ]
  playButton <- button p [ text := "Play", visible := False ]
  saveButton <- button p [ text := "Save as...", visible := False ]
  closeButton <- button p [ text := "Close", visible := False ]
  set f [ layout := margin 10 $ fill $ container p $ column 8
          [ hfill (minsize (sz 460 (-1)) (widget status))
          , hfill (widget detail)
          , hfill (widget gauge)
          , row 5 [ widget cancelButton, hglue, widget playButton, widget saveButton, widget closeButton ] ] ]

  -- Whether the recording is over, as far as this window is concerned:
  -- once it is, the timer below stops looking at it.
  finished <- newIORef False
  let closeWindow = void (windowDestroy f)
      -- Stopping the recording isn't a failure to report: and wx only
      -- destroys the window once the current event is over, so the
      -- timer can run once more in between, and would otherwise find
      -- the recording ended with 'AsyncCancelled'.
      stopRecording = writeIORef finished True >> cancel worker
  -- However the window goes (cancelled, closed, or with its project),
  -- the recording stops.
  onOwnDestroy f stopRecording
  set cancelButton [ on command := stopRecording >> closeWindow ]
  set closeButton [ on command := closeWindow ]
  set playButton [ on command := openWithDefaultApp path ]
  set saveButton [ on command := saveCopy f path ]
  WX.windowOnClose f closeWindow

  let showDone issues = do
        bytes <- getFileSize path
        set status [ text := "Your movie of " ++ quoted mjTitle ++ " is ready, in your movie player." ]
        set detail [ text := printf "%d frames, %.1f seconds, %s.%s" total
                       (fromIntegral total / fromIntegral (msFps mjSettings) :: Double)
                       (describeSize bytes)
                       (maybe "" (" " ++) (describeViewerIssues issues)) ]
        set gauge [ selection := total ]
        set f [ text := "Movie ready" ]
        set cancelButton [ visible := False ]
        mapM_ (\b -> set b [ visible := True ]) [playButton, saveButton, closeButton]
        windowReLayout f
        openWithDefaultApp path

  void $ wxTimer f [ interval := 100, enabled := True, on command := do
    alreadyDone <- readIORef finished
    unless alreadyDone $ poll worker >>= \case
      Nothing -> do
        n <- readIORef progress
        now <- getCurrentTime
        let elapsed = realToFrac (diffUTCTime now started) :: Double
            remaining = elapsed / fromIntegral n * fromIntegral (total - n)
        set gauge [ selection := n ]
        set detail [ text := printf "Frame %d of %d%s" n total
                       (if n < 2 then "" else ", " ++ describeTimeLeft remaining) ]
      Just outcome -> do
        writeIORef finished True
        case outcome of
          Right (Right issues) -> showDone issues
          Right (Left err) -> errorDialog f "Couldn't make the movie" err >> closeWindow
          Left (e :: SomeException)
            -- (stopped from here, however that came about: nothing to say)
            | Just AsyncCancelled <- fromException e -> closeWindow
            | otherwise -> errorDialog f "Couldn't make the movie" (displayException e) >> closeWindow ]

  windowShow f
  windowRaise f
  where
    describeSize :: Integer -> String
    describeSize bytes
      | bytes < 1024 * 1024 = printf "%d KB" (bytes `div` 1024)
      | otherwise = printf "%.1f MB" (fromIntegral bytes / (1024 * 1024) :: Double)
    describeTimeLeft :: Double -> String
    describeTimeLeft secs
      | secs < 60 = printf "about %d seconds left" (max 1 (round secs :: Int))
      | otherwise = printf "about %d minutes left" (round (secs / 60) :: Int)

-- | Where to record a movie: a new file in a folder of the system's
-- temporary folder, named for the project, the viewer, and the time.
movieFileName :: String -> String -> MovieFormat -> IO FilePath
movieFileName projectName viewerTitle fmt = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "FractalStream movies"
  createDirectoryIfMissing True dir
  stamp <- formatTime defaultTimeLocale "%Y-%m-%d at %H.%M.%S" <$> getZonedTime
  pure (dir </> safeFileName (projectName ++ " - " ++ viewerTitle ++ " - " ++ stamp)
            <.> movieExtension fmt)

-- | A name, minus any characters that can't go in a filename.
safeFileName :: String -> String
safeFileName = map (\c -> if c `elem` ("/\\:*?\"<>|" :: String) then '-' else c)

-- | Offer to save a copy of a movie somewhere: the Movies folder, to
-- start with, if there is one.
saveCopy :: Window a -> FilePath -> IO ()
saveCopy f path = do
  home <- getHomeDirectory
  hasMovies <- doesDirectoryExist (home </> "Movies")
  let fmt = if takeExtension' path == movieExtension AviMJPEG then AviMJPEG else QuickTimeMJPEG
      dir = if hasMovies then home </> "Movies" else takeDirectory path
  fileSaveDialog f True True "Save the movie"
    [(describeMovieFormat fmt, ["*." ++ movieExtension fmt])] dir (takeFileName path) >>= \case
      Nothing -> pure ()
      Just dest -> try (copyFile path dest) >>= \case
        Right () -> pure ()
        Left (e :: SomeException) -> errorDialog f "Couldn't save the movie" (displayException e)
  where
    takeExtension' = reverse . takeWhile (/= '.') . reverse
