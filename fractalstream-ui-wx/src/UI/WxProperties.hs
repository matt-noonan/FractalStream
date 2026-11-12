{-# language OverloadedStrings #-}

-- | The wx UI's own part of a project file, under its top-level
-- @wx-properties:@ key: where each viewer window sits and how big it
-- is, whether it can be resized or is hidden, and what part of the
-- model it is currently looking at -- plus where the configuration
-- window sits. None of this belongs to the script: the script's own
-- @center=@\/@pixelSize=@ only give a viewer's /original/ view, which
-- its "Original view" action returns to.
--
-- A 'WxLayout' is one session's record of all of this. It holds the
-- properties read from the project file, which a window applies when
-- it is first built, and a snapshot action for every window currently
-- open, which saving reads to write the properties back out.
--
-- It also keeps the windows themselves across a rebuild of the project
-- (see 'releaseViewerFrame'): a viewer whose title is still in the
-- rebuilt script, and the configuration window, keep their window, and
-- only what's inside it is rebuilt.
module UI.WxProperties
  ( ViewerProperties(..)
  , WindowProperties(..)
  , WxProperties(..)
  , readWxProperties
  , writeWxProperties
  , WxLayout
  , newWxLayout
  , restoreViewer
  , registerViewer
  , restoreConfigWindow
  , registerConfigWindow
  , captureLayout
  , rememberLayout
  , keepOnPrimaryDisplay
    -- * Windows kept across rebuilds
  , KeptFrame(..)
  , SelectedTool(..)
  , registerViewerFrame
  , releaseViewerFrame
  , takeSpareViewerFrame
  , registerConfigFrame
  , releaseConfigFrame
  , takeSpareConfigFrame
  , discardSpareFrames
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Unique
import Graphics.UI.WX (Window, Frame, StatusField, Point, Point2(..), Size2D(..), get, set, position, outerSize, Prop(..))
import Graphics.UI.WXCore.WxcClassesMZ (wxcAppDisplaySize, windowDestroy)
import Data.Foldable (forM_)
import Data.Functor (void)
import Actor.Viewer.ViewModel (History)


-- | Everything remembered about one viewer window. Every field is
-- optional, so that a hand-written entry can give just a size, say,
-- and leave the rest to the defaults.
data ViewerProperties = ViewerProperties
  { vpX         :: Maybe Int
  , vpY         :: Maybe Int
  , vpWidth     :: Maybe Int
    -- ^ The size of the viewer's image, not of its whole window
    -- (which also has a title bar, and sometimes a status bar).
  , vpHeight    :: Maybe Int
  , vpResizable :: Maybe Bool
  , vpVisible   :: Maybe Bool
  , vpCenter    :: Maybe (Double, Double)
  , vpPixelSize :: Maybe Double
  , vpAspect    :: Maybe Double
    -- ^ A real viewer's pixel height over its width, when it isn't 1.
  , vpView      :: Maybe String
    -- ^ @sphere@ or @plane@, for a complex viewer showing the other one
    -- than its script starts with.
  , vpRotation  :: Maybe [Double]
    -- ^ On the sphere, the globe's orientation (a unit quaternion: w, x,
    -- y, z). Its center and pixel size are then on the globe's screen
    -- plane.
  }

instance FromJSON ViewerProperties where
  parseJSON = withObject "viewer properties" $ \o -> ViewerProperties
    <$> o .:? "x" <*> o .:? "y"
    <*> o .:? "width" <*> o .:? "height"
    <*> o .:? "resizable" <*> o .:? "visible"
    <*> o .:? "center" <*> o .:? "pixel-size" <*> o .:? "aspect"
    <*> o .:? "view" <*> o .:? "rotation"

instance ToJSON ViewerProperties where
  toJSON ViewerProperties{..} = object $ catMaybes
    [ ("x" .=) <$> vpX, ("y" .=) <$> vpY
    , ("width" .=) <$> vpWidth, ("height" .=) <$> vpHeight
    , ("resizable" .=) <$> vpResizable, ("visible" .=) <$> vpVisible
    , ("center" .=) <$> vpCenter, ("pixel-size" .=) <$> vpPixelSize
    , ("aspect" .=) <$> vpAspect
    , ("view" .=) <$> vpView, ("rotation" .=) <$> vpRotation
    ]

-- | Where a window without anything else worth remembering sits.
data WindowProperties = WindowProperties
  { wpX :: Maybe Int
  , wpY :: Maybe Int
  }

instance FromJSON WindowProperties where
  parseJSON = withObject "window properties" $ \o ->
    WindowProperties <$> o .:? "x" <*> o .:? "y"

instance ToJSON WindowProperties where
  toJSON WindowProperties{..} = object $ catMaybes
    [ ("x" .=) <$> wpX, ("y" .=) <$> wpY ]

data WxProperties = WxProperties
  { wxViewers       :: Map String ViewerProperties
    -- ^ Keyed by viewer title, which a script already has to keep
    -- unique.
  , wxConfiguration :: Maybe WindowProperties
  }

instance FromJSON WxProperties where
  parseJSON = withObject "wx-properties" $ \o -> WxProperties
    <$> o .:? "viewers" .!= Map.empty
    <*> o .:? "configuration"

instance ToJSON WxProperties where
  toJSON WxProperties{..} = object $ catMaybes
    [ if Map.null wxViewers then Nothing else Just ("viewers" .= wxViewers)
    , ("configuration" .=) <$> wxConfiguration
    ]

noWxProperties :: WxProperties
noWxProperties = WxProperties Map.empty Nothing

-- | Read the @wx-properties:@ key out of a project file's
-- frontend-specific keys ('Actor.Ensemble.ensembleFrontendProperties').
-- A file without one just has no properties to restore.
readWxProperties :: KM.KeyMap Value -> Either String WxProperties
readWxProperties = maybe (Right noWxProperties) (parseEither parseJSON) . KM.lookup "wx-properties"

-- | Replace the @wx-properties:@ key, leaving any other frontend's
-- keys alone.
writeWxProperties :: WxProperties -> KM.KeyMap Value -> KM.KeyMap Value
writeWxProperties = KM.insert "wx-properties" . toJSON

data WxLayout = WxLayout
  { wlSaved   :: IORef WxProperties
  , wlViewers :: IORef (Map String (Unique, IO ViewerProperties))
  , wlConfig  :: IORef (Maybe (Unique, IO WindowProperties))
  , wlCarried :: IORef (Set String)
    -- ^ The viewers whose entry in 'wlSaved' was captured from a window
    -- in this very session, rather than read from the project file.
    -- Their positions are known to be on a display that's attached, so
    -- unlike the file's, they aren't pulled onto the primary display
    -- ('keepOnPrimaryDisplay'): rebuilding a viewer on a second display
    -- leaves it there.
  , wlConfigCarried :: IORef Bool
    -- ^ The same, for the configuration window.
  , wlLiveFrames  :: IORef (Map String (Unique, IO KeptFrame))
    -- ^ Each viewer window, by title, with the action that empties it
    -- (see 'registerViewerFrame').
  , wlSpareFrames :: IORef (Map String KeptFrame)
    -- ^ Emptied viewer windows, waiting for their rebuilt viewer.
  , wlLiveConfig  :: IORef (Maybe (Unique, IO (Frame ())))
  , wlSpareConfig :: IORef (Maybe (Frame ()))
  }

newWxLayout :: WxProperties -> IO WxLayout
newWxLayout props = WxLayout <$> newIORef props <*> newIORef Map.empty <*> newIORef Nothing
                             <*> newIORef Set.empty <*> newIORef False
                             <*> newIORef Map.empty <*> newIORef Map.empty
                             <*> newIORef Nothing <*> newIORef Nothing

-- | A viewer's window, emptied so that its rebuilt viewer can use it:
-- the window stays where it is, as it is (on screen, in front of or
-- behind others), and only what's in it is replaced.
data KeptFrame = KeptFrame
  { kfFrame     :: Frame ()
  , kfResizable :: Bool
    -- ^ (a window can't change this, so a viewer that now can or can't
    -- be resized needs a new one)
  , kfStatus    :: StatusField
    -- ^ the window's status bar's field (a window only gets one status
    -- bar, so the rebuilt viewer uses this one)
  , kfTool      :: SelectedTool
    -- ^ the tool the viewer had selected, to select again
  , kfHistory   :: History
    -- ^ the viewer's history of views, for "Last view" and "Next view"
  }

-- | The tool a viewer had selected, and for one of its script's tools,
-- where that tool's settings window was (if it had one, and it was
-- showing).
data SelectedTool
  = SelectedNavigate
  | SelectedDebug
  | SelectedMoviePath
  | SelectedTool String (Maybe Point)

-- | Record a viewer's window, with the action that empties it and gives
-- it back for reuse ('releaseViewerFrame'). Returns the action that
-- forgets it again, for when the window itself goes; that leaves alone
-- any newer registration for the same title.
registerViewerFrame :: WxLayout -> String -> IO KeptFrame -> IO (IO ())
registerViewerFrame WxLayout{..} title release = do
  token <- newUnique
  modifyIORef' wlLiveFrames (Map.insert title (token, release))
  pure $ modifyIORef' wlLiveFrames $ \live -> case Map.lookup title live of
    Just (token', _) | token' == token -> Map.delete title live
    _ -> live

-- | Empty the window of the viewer with this title, if it has one, and
-- keep it for the viewer's next build ('takeSpareViewerFrame'). Gives
-- the window back, so that a caller sweeping away a project's windows
-- can leave it alone.
releaseViewerFrame :: WxLayout -> String -> IO (Maybe (Frame ()))
releaseViewerFrame WxLayout{..} title = do
  live <- readIORef wlLiveFrames
  case Map.lookup title live of
    Nothing -> pure Nothing
    Just (_, release) -> do
      modifyIORef' wlLiveFrames (Map.delete title)
      kept <- release
      modifyIORef' wlSpareFrames (Map.insert title kept)
      pure (Just (kfFrame kept))

-- | The emptied window kept for the viewer with this title, if there is
-- one (it's then no longer spare).
takeSpareViewerFrame :: WxLayout -> String -> IO (Maybe KeptFrame)
takeSpareViewerFrame WxLayout{..} title = do
  spare <- Map.lookup title <$> readIORef wlSpareFrames
  modifyIORef' wlSpareFrames (Map.delete title)
  pure spare

-- | 'registerViewerFrame', for the configuration window.
registerConfigFrame :: WxLayout -> IO (Frame ()) -> IO (IO ())
registerConfigFrame WxLayout{..} release = do
  token <- newUnique
  writeIORef wlLiveConfig (Just (token, release))
  pure $ modifyIORef' wlLiveConfig $ \case
    Just (token', _) | token' == token -> Nothing
    other -> other

-- | 'releaseViewerFrame', for the configuration window.
releaseConfigFrame :: WxLayout -> IO (Maybe (Frame ()))
releaseConfigFrame WxLayout{..} = readIORef wlLiveConfig >>= \case
  Nothing -> pure Nothing
  Just (_, release) -> do
    writeIORef wlLiveConfig Nothing
    f <- release
    writeIORef wlSpareConfig (Just f)
    pure (Just f)

-- | 'takeSpareViewerFrame', for the configuration window.
takeSpareConfigFrame :: WxLayout -> IO (Maybe (Frame ()))
takeSpareConfigFrame WxLayout{..} = do
  spare <- readIORef wlSpareConfig
  writeIORef wlSpareConfig Nothing
  pure spare

-- | Destroy the emptied windows no rebuilt viewer took (one whose build
-- failed, say), and the configuration window's, if it wasn't taken.
discardSpareFrames :: WxLayout -> IO ()
discardSpareFrames WxLayout{..} = do
  spares <- readIORef wlSpareFrames
  writeIORef wlSpareFrames Map.empty
  forM_ spares (void . windowDestroy . kfFrame)
  readIORef wlSpareConfig >>= mapM_ (void . windowDestroy)
  writeIORef wlSpareConfig Nothing

-- | The properties a viewer window being built for the given title
-- should start from, if any are remembered for it, and whether its
-- window should then be kept on the primary display (see 'wlCarried').
restoreViewer :: WxLayout -> String -> IO (Maybe ViewerProperties, Bool)
restoreViewer WxLayout{..} title = do
  props <- Map.lookup title . wxViewers <$> readIORef wlSaved
  carried <- Set.member title <$> readIORef wlCarried
  pure (props, not carried)

-- | Record a newly-built viewer window's snapshot action. Returns two
-- actions for when the window goes away, each of which leaves alone
-- any newer window registered under the same title since:
--
--   * the first remembers the window's current state as the
--     properties to restore, then forgets the window -- for a window
--     about to be replaced by a new one for the same viewer (a viewer
--     rebuilds itself when the configuration's variables change), so
--     that the new window picks up exactly where the old one left off;
--
--   * the second just forgets the window, for when it has been
--     destroyed and can't be asked about its state any more.
registerViewer :: WxLayout -> String -> IO ViewerProperties -> IO (IO (), IO ())
registerViewer WxLayout{..} title snapshot = do
  token <- newUnique
  modifyIORef' wlViewers (Map.insert title (token, snapshot))
  let isCurrent = (\case { Just (token', _) -> token' == token; Nothing -> False })
                . Map.lookup title <$> readIORef wlViewers
      forget = isCurrent >>= \current -> when current $
        modifyIORef' wlViewers (Map.delete title)
      remember = isCurrent >>= \current -> when current $ do
        props <- snapshot
        modifyIORef' wlSaved $ \saved ->
          saved { wxViewers = Map.insert title props (wxViewers saved) }
        modifyIORef' wlCarried (Set.insert title)
        forget
  pure (remember, forget)

-- | Like 'restoreViewer', for the configuration window.
restoreConfigWindow :: WxLayout -> IO (Maybe WindowProperties, Bool)
restoreConfigWindow WxLayout{..} = do
  props <- wxConfiguration <$> readIORef wlSaved
  carried <- readIORef wlConfigCarried
  pure (props, not carried)

registerConfigWindow :: WxLayout -> IO WindowProperties -> IO (IO ())
registerConfigWindow WxLayout{..} snapshot = do
  token <- newUnique
  writeIORef wlConfig (Just (token, snapshot))
  pure $ modifyIORef' wlConfig $ \case
    Just (token', _) | token' == token -> Nothing
    other -> other

-- | The properties to save: the open windows' current state. Viewers
-- without an open window (one renamed or removed from the script, say)
-- are left out, so stale entries don't pile up in the file.
captureLayout :: WxLayout -> IO WxProperties
captureLayout WxLayout{..} = do
  viewers <- traverse snd =<< readIORef wlViewers
  config <- readIORef wlConfig >>= traverse snd
  saved <- readIORef wlSaved
  pure WxProperties
    { wxViewers = viewers
    , wxConfiguration = config <|> wxConfiguration saved
    }

-- | Capture every open window's state as the properties to restore,
-- then forget the windows themselves -- for just before tearing every
-- window down to rebuild the project, so that the rebuilt windows come
-- back the way they were. A viewer that has no window just now keeps
-- whatever was remembered for it before (such as the file's layout for
-- a project whose script didn't build, and so has no windows at all
-- until it's fixed).
rememberLayout :: WxLayout -> IO ()
rememberLayout wl@WxLayout{..} = do
  live <- captureLayout wl
  saved <- readIORef wlSaved
  writeIORef wlSaved live { wxViewers = Map.union (wxViewers live) (wxViewers saved) }
  modifyIORef' wlCarried (Set.union (Map.keysSet (wxViewers live)))

  readIORef wlConfig >>= \case
    Just _  -> writeIORef wlConfigCarried True
    Nothing -> pure ()
  writeIORef wlViewers Map.empty
  writeIORef wlConfig Nothing

-- | Move a window, if need be, so that it fits on the primary display
-- (or at least starts at its top left corner, if it's too big to fit).
-- A position read from a project file can be anywhere at all -- the
-- project might have been saved with a second display attached, perhaps
-- a projector, or on another computer entirely -- and a window restored
-- there would be invisible and unreachable. This does mean windows
-- can't stay on a second display from one session to the next:
-- wxHaskell only tells us the primary display's size, so there's no
-- telling whether some other display is attached that would show the
-- window where it was.
keepOnPrimaryDisplay :: Window a -> IO ()
keepOnPrimaryDisplay w = do
  Size displayW displayH <- wxcAppDisplaySize
  Point x y <- get w position
  Size windowW windowH <- get w outerSize
  let clamp hi = max 0 . min hi
      x' = clamp (displayW - windowW) x
      y' = clamp (displayH - windowH) y
  when ((x', y') /= (x, y)) $ set w [ position := Point x' y' ]
