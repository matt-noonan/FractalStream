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
import Graphics.UI.WX (Window, Point2(..), Size2D(..), get, set, position, outerSize, Prop(..))
import Graphics.UI.WXCore.WxcClassesMZ (wxcAppDisplaySize)


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
  }

instance FromJSON ViewerProperties where
  parseJSON = withObject "viewer properties" $ \o -> ViewerProperties
    <$> o .:? "x" <*> o .:? "y"
    <*> o .:? "width" <*> o .:? "height"
    <*> o .:? "resizable" <*> o .:? "visible"
    <*> o .:? "center" <*> o .:? "pixel-size"

instance ToJSON ViewerProperties where
  toJSON ViewerProperties{..} = object $ catMaybes
    [ ("x" .=) <$> vpX, ("y" .=) <$> vpY
    , ("width" .=) <$> vpWidth, ("height" .=) <$> vpHeight
    , ("resizable" .=) <$> vpResizable, ("visible" .=) <$> vpVisible
    , ("center" .=) <$> vpCenter, ("pixel-size" .=) <$> vpPixelSize
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
  }

newWxLayout :: WxProperties -> IO WxLayout
newWxLayout props = WxLayout <$> newIORef props <*> newIORef Map.empty <*> newIORef Nothing
                             <*> newIORef Set.empty <*> newIORef False

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
-- back the way they were.
rememberLayout :: WxLayout -> IO ()
rememberLayout wl@WxLayout{..} = do
  props <- captureLayout wl
  writeIORef wlSaved props
  writeIORef wlCarried (Map.keysSet (wxViewers props))
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
