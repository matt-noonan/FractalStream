{-# language OverloadedStrings #-}
-- | Autocolor model: tables that give each distinct point a color of its
-- own, the first time it's seen, for coloring by points that aren't known
-- in advance (the roots a Newton's method converges to, say). Scripts use
-- them with @autocolor(group, point)@, which adds a point that isn't in the
-- group's table yet; @nearest_autocolor(group, point)@, which doesn't (a
-- point that isn't near an entry gets the group's miss color); and
-- @get_autocolor(group, point)@, which doesn't either, but gives a point
-- that isn't near an entry a blend of all the entries' colors, weighted by
-- how near they are (more useful when a group is locked, or a viewer only
-- reads it).
--
-- Each group is a table of points and their colors, numbered as scripts
-- name them; viewers that name the same group share its colors (a
-- parameter plane and a dynamical plane, say). A point matches the nearest
-- entry within the group's tolerance. A new point gets the next hue in a
-- golden-angle sequence, so which point gets which color depends on the
-- order points are first seen in -- which, with renders split into tiles
-- over several threads, isn't the same from one run to the next. Saving a
-- project saves its tables, and a locked group adds no new points, which
-- makes the colors stay put.
module Actor.Autocolor
  ( AutocolorState
  , newAutocolorState
  , AutocolorGroup(..)
  , AutocolorGroupId(..)
  , AutocolorLookup(..)
  , autocolorLookup
  , autocolorLookupFromFunction
  , autocolorCacheLookup
  , autocolorEdits
  , AutocolorMode(..)
  , autocolor
  , nearestAutocolor
  , blendAutocolor
  , maybeGetAutocolor
  , autocolorGroup
  , autocolorGroups
  , autocolorGroupsDynamic
  , makeEmptyAutocolorGroup
  , addAutocolorGroup
  , deleteAutocolorGroup
  , deleteAutocolor
  , setAutocolorEntryColor
  , addAutocolorEntry
  , moveAutocolorEntry
  , clearAutocolorGroup
  , showAutocolorPoint
  , setAutocolorTolerance
  , setAutocolorLocked
  , setAutocolorMissColor
  , setAutocolorBlendMisses
  , maxAutocolorEntries
  , writeAutocolorState
  , readAutocolorState
  ) where

import FractalStream.Prelude

import Data.Color
import Data.DynamicValue
import Control.Concurrent
import Data.Aeson (Value(..), (.=), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither, withObject)
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Text as Text
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Text.Printf (printf)
import qualified Data.Aeson.Types as Aeson (Parser)
import Language.Type (TypeProxy(..), HaskellType, toUserString, AutocolorMode(..), autocolorModeCode)
import Language.Value.Parser (parseConstant)

import qualified Data.Map as Map
import Data.IORef
import Data.Bits (shiftL, (.|.))
import Foreign (Ptr, FunPtr, ForeignPtr, nullPtr, mallocBytes, pokeByteOff, freeHaskellFunPtr, withForeignPtr)
import qualified Foreign.Concurrent as FC
import Foreign.C.Types (CDouble(..))
import System.IO (fixIO)

newtype AutocolorGroupId = AutocolorGroupId Int
  deriving (Eq, Ord, Show)

-- | Every autocolor group in a project.
data AutocolorState = AutocolorState
  { acGroups :: Variable (Map AutocolorGroupId AutocolorGroup)
  , acNextGroupId :: MVar Int
  , acGroupsMutex :: MVar ()
    -- ^ held to add a group, so that two renderers looking for the same
    -- new group don't each add one
  , acEdits :: Variable Int
    -- ^ counts changes to the colors (see 'autocolorEdits')
  , acCache :: ForeignPtr ()
    -- ^ the snapshot directory compiled code reads (see 'autocolorCacheLookup')
  , acPublishLock :: MVar ()
    -- ^ held to publish a snapshot, so that the latest state always wins
  , acAddPending :: IORef Bool
    -- ^ whether a redraw for newly added points is already on its way
  }

data AutocolorGroup = AutocolorGroup
  { agTolerance :: Variable Double
  , agLocked :: Variable Bool
  , agBlendMisses :: Variable Bool
    -- ^ whether a locked (or full) group gives @autocolor@'s points that
    -- aren't near an entry a blend of its colors (as @get_autocolor@
    -- does), rather than its miss color
  , agMissColor :: Variable Color
    -- ^ the color of a point that isn't in a locked (or full) group's table
  , agNextColorAngle :: Variable Double
  , agTable :: Variable (Map (Double, Double) (Variable Color))
  , agTableWriteMutex :: MVar ()
  -- ^ the key set of agTable should not be changed without taking this mutex
  , agFullWarned :: Variable Bool
  , agAdded :: IORef (IO ())
    -- ^ run after a point is added to the table (by its state: publish a
    -- new snapshot, and redraw)
  }

-- | How compiled code looks colors up: how (see 'AutocolorMode'), the
-- group's number, and the point.
--
-- Compiled code calls 'autocolorCacheLookup' (C, @fs_ac_lookup@) with
-- 'autocolorCache', which finds the colors of points a group already has
-- in a snapshot, without calling back into Haskell or taking any lock;
-- only a point to add goes on to 'autocolorFunction'.
data AutocolorLookup = AutocolorLookup
  { autocolorFunction :: AutocolorMode -> Int -> Double -> Double -> IO Color
  , autocolorCache    :: ForeignPtr ()
  }

foreign import ccall "&fs_ac_lookup"
  autocolorCacheLookup :: FunPtr (Ptr () -> Word8 -> Int32 -> CDouble -> CDouble -> IO Word32)

foreign import ccall "fs_ac_publish"
  publishSnapshot :: Ptr () -> Ptr () -> IO ()

foreign import ccall "fs_ac_new"
  newDirectory :: FunPtr (Word8 -> Int32 -> CDouble -> CDouble -> IO Word32) -> IO (Ptr ())

foreign import ccall "fs_ac_free"
  freeDirectory :: Ptr () -> IO ()

foreign import ccall "wrapper"
  wrapAddFunction :: (Word8 -> Int32 -> CDouble -> CDouble -> IO Word32)
                  -> IO (FunPtr (Word8 -> Int32 -> CDouble -> CDouble -> IO Word32))

-- | A snapshot directory, with no snapshot yet (so that every lookup goes
-- to the function). It's freed, with its current snapshot and the
-- function's callback, once nothing refers to it -- by then no compiled
-- code can be using it either, since that only happens inside
-- 'withForeignPtr'.
newCache :: (AutocolorMode -> Int -> Double -> Double -> IO Color) -> IO (ForeignPtr ())
newCache f = do
  fp <- wrapAddFunction $ \code n (CDouble x) (CDouble y) ->
    packColor <$> f (modeOf code) (fromIntegral n) x y
  dir <- newDirectory fp
  when (dir == nullPtr) (fail "out of memory for an autocolor directory")
  FC.newForeignPtr dir (freeDirectory dir >> freeHaskellFunPtr fp)

packColor :: Color -> Word32
packColor c = let (r, g, b) = colorToRGB c
              in shiftL (fromIntegral r) 16 .|. shiftL (fromIntegral g) 8 .|. fromIntegral b

-- | The mode compiled code passed (see 'autocolorModeCode').
modeOf :: Word8 -> AutocolorMode
modeOf code = fromMaybe NearestPoint (lookup (fromIntegral code) [ (autocolorModeCode m, m) | m <- [minBound .. maxBound] ])

-- | A lookup that always calls the given function (for tests).
autocolorLookupFromFunction :: (AutocolorMode -> Int -> Double -> Double -> IO Color) -> IO AutocolorLookup
autocolorLookupFromFunction f = AutocolorLookup f <$> newCache f

-- | The most points a group's table holds. A script that gives
-- @autocolor@ points that never repeat (every pixel's own @z@, say) would
-- otherwise add an entry for every pixel, slowing every lookup down; a
-- full group acts as if it were locked.
maxAutocolorEntries :: Int
maxAutocolorEntries = 256

newAutocolorState :: MonadIO io => io AutocolorState
newAutocolorState = liftIO $ fixIO $ \st -> do
  groups <- newVariable Map.empty
  nextId <- newMVar 0
  groupsMutex <- newMVar ()
  edits <- newVariable 0
  cache <- newCache (lookupColor st)
  publishLock <- newMVar ()
  pending <- newIORef False
  pure (AutocolorState groups nextId groupsMutex edits cache publishLock pending)

-- | A 'Dynamic' that changes whenever colors may have changed, so that
-- viewers using them can be redrawn: an entry recolored, moved, or
-- deleted; a group's tolerance, lock, or miss color changed; a group
-- cleared or deleted -- and points added. (Points added during a render
-- are counted together, shortly after the first: a render that finds a
-- few new points causes one more render of each viewer, not one for each
-- point. The viewer that found them finds no more the second time.)
autocolorEdits :: AutocolorState -> Dynamic Int
autocolorEdits = dyn . acEdits

bumpEdits :: AutocolorState -> IO ()
bumpEdits st = publishAutocolor st >> modifyValue (acEdits st) (+ 1)

-- | After a point is added: publish it, and redraw soon (once for every
-- point added in the meantime).
pointAdded :: AutocolorState -> IO ()
pointAdded st = do
  publishAutocolor st
  alreadyPending <- atomicModifyIORef' (acAddPending st) (True,)
  unless alreadyPending $ void $ forkIO $ do
    threadDelay 200000
    writeIORef (acAddPending st) False
    modifyValue (acEdits st) (+ 1)

-- | Publish a snapshot of every group for compiled code (see
-- @autocolor_cache.c@ for its layout, and for how the snapshot it
-- replaces is freed once no lookup can still be reading it).
publishAutocolor :: AutocolorState -> IO ()
publishAutocolor AutocolorState{..} = withMVar acPublishLock $ \_ -> do
  groups <- Map.toList <$> getDynamic acGroups
  infos <- forM groups $ \(AutocolorGroupId n, AutocolorGroup{..}) -> do
    tol <- getDynamic agTolerance
    locked <- getDynamic agLocked
    miss <- getDynamic agMissColor
    blendMisses <- getDynamic agBlendMisses
    entries <- getDynamic agTable >>= traverse getDynamic
    pure ( n, not locked && Map.size entries < maxAutocolorEntries, blendMisses, miss, tol
         , Map.toList entries )
  let headerSize = 8
      groupSize = 40
      entrySize = 24
      entriesStart = headerSize + groupSize * length infos
      offsets = scanl (+) entriesStart [ entrySize * length es | (_, _, _, _, _, es) <- infos ]
      total = last offsets
  buf <- mallocBytes total
  pokeByteOff buf 0 (fromIntegral (length infos) :: Int32)
  pokeByteOff buf 4 (0 :: Int32)
  forM_ (zip3 [0 ..] infos offsets) $ \(k, (n, canAdd, blendMisses, miss, tol, es), off) -> do
    let g = headerSize + groupSize * k
    pokeByteOff buf g (fromIntegral n :: Int32)
    pokeByteOff buf (g + 4) (if canAdd then 1 else 0 :: Int32)
    pokeByteOff buf (g + 8) (packColor miss)
    pokeByteOff buf (g + 12) (fromIntegral (length es) :: Int32)
    pokeByteOff buf (g + 16) (CDouble (tol * tol))
    pokeByteOff buf (g + 24) (fromIntegral off :: Int64)
    pokeByteOff buf (g + 32) (if blendMisses then 1 else 0 :: Int32)
    pokeByteOff buf (g + 36) (0 :: Int32)
    forM_ (zip [0 ..] es) $ \(i, ((x, y), c)) -> do
      let e = off + entrySize * i
      pokeByteOff buf e (CDouble x)
      pokeByteOff buf (e + 8) (CDouble y)
      pokeByteOff buf (e + 16) (packColor c)
      pokeByteOff buf (e + 20) (0 :: Word32)
  withForeignPtr acCache $ \dir -> publishSnapshot dir buf

makeEmptyAutocolorGroup :: MonadIO io => io AutocolorGroup
makeEmptyAutocolorGroup = liftIO $ do
  agTolerance <- newVariable 0.001
  agLocked <- newVariable False
  agMissColor <- newVariable grey
  agBlendMisses <- newVariable True
  agNextColorAngle <- newVariable 0
  agTable <- newVariable Map.empty
  agTableWriteMutex <- newMVar ()
  agFullWarned <- newVariable False
  agAdded <- newIORef (pure ())
  pure AutocolorGroup{..}

addAutocolorGroup :: MonadIO io => AutocolorState -> io AutocolorGroupId
addAutocolorGroup st@AutocolorState{..} = liftIO $ do
  gId <- withMVar acGroupsMutex $ \_ -> do
    groups <- getDynamic acGroups
    let used = Map.keys groups
    gId <- modifyMVar acNextGroupId $ \n ->
      let n' = until (\k -> AutocolorGroupId k `notElem` used) (+ 1) n
      in pure (n' + 1, AutocolorGroupId n')
    group <- newGroupOf st
    modifyValue acGroups (Map.insert gId group)
    pure gId
  publishAutocolor st
  pure gId

-- | An empty group belonging to this state.
newGroupOf :: AutocolorState -> IO AutocolorGroup
newGroupOf st = do
  g <- makeEmptyAutocolorGroup
  writeIORef (agAdded g) (pointAdded st)
  pure g

deleteAutocolorGroup :: MonadIO io => AutocolorState -> AutocolorGroupId -> io ()
deleteAutocolorGroup st@AutocolorState{..} gId = liftIO $ do
  withMVar acGroupsMutex $ \_ ->
    modifyValue acGroups (Map.filterWithKey (\gId' _ -> gId /= gId'))
  bumpEdits st

-- | The group with this number, adding an empty one if there isn't one.
autocolorGroup :: MonadIO io => AutocolorState -> AutocolorGroupId -> io AutocolorGroup
autocolorGroup st@AutocolorState{..} gId = liftIO $
  Map.lookup gId <$> getDynamic acGroups >>= \case
    Just g -> pure g
    Nothing -> do
      (g, isNew) <- withMVar acGroupsMutex $ \_ ->
        -- (again: someone else may have added it while we waited)
        Map.lookup gId <$> getDynamic acGroups >>= \case
          Just g -> pure (g, False)
          Nothing -> do
            g <- newGroupOf st
            modifyValue acGroups (Map.insert gId g)
            pure (g, True)
      when isNew (publishAutocolor st)
      pure g

autocolorGroups :: MonadIO io => AutocolorState -> io (Map AutocolorGroupId AutocolorGroup)
autocolorGroups = liftIO . getDynamic . acGroups

-- | The groups, as they change (a script adds one the first time it uses
-- it).
autocolorGroupsDynamic :: AutocolorState -> Dynamic (Map AutocolorGroupId AutocolorGroup)
autocolorGroupsDynamic = dyn . acGroups

-- | The lookup to give compiled code.
autocolorLookup :: AutocolorState -> AutocolorLookup
autocolorLookup st = AutocolorLookup (lookupColor st) (acCache st)

lookupColor :: AutocolorState -> AutocolorMode -> Int -> Double -> Double -> IO Color
lookupColor st mode n x y = do
  g <- autocolorGroup st (AutocolorGroupId n)
  case mode of
    AddPoint -> autocolor g x y
    NearestPoint -> nearestAutocolor g x y
    BlendPoints -> blendAutocolor g x y

-- | Find the color for this (x,y) pair, or the group's miss color if (x,y)
-- is not within the tolerance of any known point in the autocolor table
-- (@nearest_autocolor@).
nearestAutocolor :: MonadIO io => AutocolorGroup -> Double -> Double -> io Color
nearestAutocolor ag x y = liftIO $ maybeGetAutocolor ag x y >>= \case
  Just c  -> pure c
  Nothing -> getDynamic (agMissColor ag)

-- | Find the color for this (x,y) pair; or, if (x,y) is not within the
-- tolerance of any known point, a blend of every point's color, each
-- weighted by one over its squared distance from (x,y); or the group's
-- miss color if it has no points (@get_autocolor@). The same as
-- @fs_ac_lookup@ computes, in @autocolor_cache.c@.
blendAutocolor :: MonadIO io => AutocolorGroup -> Double -> Double -> io Color
blendAutocolor ag x y = liftIO $ maybeGetAutocolor ag x y >>= \case
  Just c -> pure c
  Nothing -> do
    entries <- getDynamic (agTable ag) >>= traverse getDynamic
    if Map.null entries then getDynamic (agMissColor ag) else do
      let weighted = [ (1 / ((cx - x)^(2 :: Int) + (cy - y)^(2 :: Int)), colorToRGB c)
                     | ((cx, cy), c) <- Map.toList entries ]
          total = sum (map fst weighted)
          -- (rounded half up, as the C does)
          channel f = fromIntegral (max 0 (min 255 (floor
            (sum [ w * fromIntegral (f rgb) | (w, rgb) <- weighted ] / total + 0.5) :: Int)))
          fst3 (a, _, _) = a
          snd3 (_, b, _) = b
          thd3 (_, _, c) = c
      pure (rgbToColor (channel fst3, channel snd3, channel thd3))

-- | Find the color of the nearest point in the autocolor table to (x,y),
-- or Nothing if none is within the tolerance.
maybeGetAutocolor :: MonadIO io => AutocolorGroup -> Double -> Double -> io (Maybe Color)
maybeGetAutocolor AutocolorGroup{..} x y = liftIO $ do
  r2 <- (\r -> r * r) <$> getDynamic agTolerance
  m <- Map.toList <$> getDynamic agTable
  let dist2 ((cx, cy), _) = (cx - x)*(cx - x) + (cy - y)*(cy - y)
  case filter ((<= r2) . dist2) m of
    [] -> pure Nothing
    hits -> Just <$> getDynamic (snd (minimumBy (comparing dist2) hits))

-- | Delete the autocolor table entry with *exactly* these (x,y) coordinates.
deleteAutocolor :: MonadIO io => AutocolorState -> AutocolorGroup -> Double -> Double -> io ()
deleteAutocolor st AutocolorGroup{..} x y = liftIO $ do
  withMVar agTableWriteMutex $ \_ ->
    modifyValue agTable $ Map.filterWithKey (\p _ -> (x,y) /= p)
  bumpEdits st

-- | Find the color for this point, adding it to the table with the next
-- color if it isn't there and the group isn't locked (or full). In a
-- locked (or full) group, a point that isn't near an entry gets a blend
-- of the group's colors if it blends misses ('agBlendMisses'), and its
-- miss color if not.
--
-- The table is read without the lock first: once a group knows its points,
-- nearly every lookup finds one, and render threads shouldn't queue up on a
-- lock for it. A miss takes the lock and looks again before adding the
-- point, so two renderers that both missed can't add nearly the same point
-- twice: whichever gets the lock second finds the first one's entry.
autocolor :: MonadIO io => AutocolorGroup -> Double -> Double -> io Color
autocolor ag@AutocolorGroup{..} x y = liftIO $ maybeGetAutocolor ag x y >>= \case
  Just c -> pure c
  Nothing -> do
    (c0, added) <- withMVar agTableWriteMutex $ \_ -> do
      isLocked <- getDynamic agLocked
      size <- Map.size <$> getDynamic agTable
      maybeGetAutocolor ag x y >>= \case
        Just c  -> pure (c, False)
        Nothing
          | isLocked || size >= maxAutocolorEntries -> do
              blendMisses <- getDynamic agBlendMisses
              (, False) <$> if blendMisses then blendAutocolor ag x y
                                           else getDynamic agMissColor
          | otherwise -> do
              angle <- getDynamic agNextColorAngle
              modifyValue agNextColorAngle (+ 0.3821592356687898)
              let c0 = hsvToColor (angle, 1, 1)
              c <- newVariable c0
              modifyValue agTable (Map.insert (x, y) c)
              -- (warned of once it fills up: after that, compiled code
              -- doesn't even ask)
              when (size + 1 == maxAutocolorEntries) $ do
                warned <- getDynamic agFullWarned
                unless warned $ do
                  setValue agFullWarned True
                  hPutStrLn stderr ("warning: an autocolor group has " ++ show maxAutocolorEntries
                                    ++ " points now, so no more will be added; the points \
                                       \given to autocolor may never repeat")
              pure (c0, True)
    when added (join (readIORef agAdded))
    pure c0

setAutocolorEntryColor :: MonadIO io => AutocolorState -> AutocolorGroup -> (Double, Double) -> Color -> io ()
setAutocolorEntryColor st AutocolorGroup{..} p c = liftIO $ do
  Map.lookup p <$> getDynamic agTable >>= mapM_ (`setValue` c)
  bumpEdits st

setAutocolorTolerance :: MonadIO io => AutocolorState -> AutocolorGroup -> Double -> io ()
setAutocolorTolerance st ag r = liftIO (setValue (agTolerance ag) r >> bumpEdits st)

-- | Lock or unlock a group. (This counts as an edit: points that a
-- locked group gave its miss color can get colors of their own once it's
-- unlocked and they're drawn again.)
setAutocolorLocked :: MonadIO io => AutocolorState -> AutocolorGroup -> Bool -> io ()
setAutocolorLocked st ag b = liftIO (setValue (agLocked ag) b >> bumpEdits st)

-- | Add an entry by hand, with the given color or else the next one,
-- replacing any entry at exactly that point. Returns its color.
addAutocolorEntry :: MonadIO io => AutocolorState -> AutocolorGroup -> (Double, Double) -> Maybe Color -> io Color
addAutocolorEntry st AutocolorGroup{..} p mc = liftIO $ do
  c0 <- withMVar agTableWriteMutex $ \_ -> do
    c0 <- case mc of
      Just c -> pure c
      Nothing -> do
        angle <- getDynamic agNextColorAngle
        modifyValue agNextColorAngle (+ 0.3821592356687898)
        pure (hsvToColor (angle, 1, 1))
    cv <- newVariable c0
    modifyValue agTable (Map.insert p cv)
    pure c0
  bumpEdits st
  pure c0

-- | Move an entry to another point, keeping its color.
moveAutocolorEntry :: MonadIO io => AutocolorState -> AutocolorGroup -> (Double, Double) -> (Double, Double) -> io ()
moveAutocolorEntry st AutocolorGroup{..} from to = liftIO $ do
  withMVar agTableWriteMutex $ \_ -> modifyValue agTable $ \m -> case Map.lookup from m of
    Nothing -> m
    Just cv -> Map.insert to cv (Map.delete from m)
  bumpEdits st

-- | Delete every entry in a group (and start its colors over).
clearAutocolorGroup :: MonadIO io => AutocolorState -> AutocolorGroup -> io ()
clearAutocolorGroup st AutocolorGroup{..} = liftIO $ do
  withMVar agTableWriteMutex $ \_ -> do
    setValue agTable Map.empty
    setValue agNextColorAngle 0
    setValue agFullWarned False
  bumpEdits st

-- | A point as a complex number literal, as the project file and the
-- autocolor editor write it: @0.1812324445 - 1.0839541013i@. Rounded to 10
-- decimal places (much finer than any tolerance), and in plain ASCII
-- (libyaml quotes and escapes text with 𝑖 in it).
showAutocolorPoint :: (Double, Double) -> String
showAutocolorPoint (x, y)
  | y' == 0 = num x'
  | x' == 0 = num y' ++ "i"
  | y' < 0 = num x' ++ " - " ++ num (negate y') ++ "i"
  | otherwise = num x' ++ " + " ++ num y' ++ "i"
  where
    rnd :: Double -> Double
    -- (less than 1e-9 is rounding noise in a root's coordinates)
    rnd v | abs v < 1e-9 = 0
          | otherwise = fromIntegral (round (v * 1e10) :: Integer) / 1e10
    x' = rnd x
    y' = rnd y
    num v = let s0 = printf "%.10f" v :: String
                s1 = reverse (dropWhile (== '0') (reverse s0))
            in if last s1 == '.' then init s1 else s1

-- | Whether a locked (or full) group blends its colors for @autocolor@'s
-- points that aren't near an entry (see 'agBlendMisses').
setAutocolorBlendMisses :: MonadIO io => AutocolorState -> AutocolorGroup -> Bool -> io ()
setAutocolorBlendMisses st ag b = liftIO (setValue (agBlendMisses ag) b >> bumpEdits st)

setAutocolorMissColor :: MonadIO io => AutocolorState -> AutocolorGroup -> Color -> io ()
setAutocolorMissColor st ag c = liftIO (setValue (agMissColor ag) c >> bumpEdits st)

------------------------------------------------------------
-- Saving
------------------------------------------------------------

-- | The project file's @autocolor:@ key, by group number:
--
-- > autocolor:
-- >   '0':
-- >     tolerance: 0.001
-- >     locked: true
-- >     next-hue: 1.146
-- >     miss-color: rgb(0.502, 0.502, 0.502)
-- >     entries:
-- >     - point: 1.1673039780
-- >       color: rgb(1.000, 0.000, 0.000)
-- >     - point: 0.1812324445 - 1.0839541013i
-- >       color: rgb(0.000, 0.827, 1.000)
--
-- Points and colors are written, and read, as the script language writes
-- them.
--
-- 'Nothing' when there are no groups.
writeAutocolorState :: MonadIO io => AutocolorState -> io (Maybe Value)
writeAutocolorState AutocolorState{..} = liftIO $ do
  groups <- Map.toList <$> getDynamic acGroups
  if null groups then pure Nothing else do
    entries <- forM groups $ \(AutocolorGroupId n, AutocolorGroup{..}) -> do
      tol <- getDynamic agTolerance
      locked <- getDynamic agLocked
      hue <- getDynamic agNextColorAngle
      miss <- getDynamic agMissColor
      blendMisses <- getDynamic agBlendMisses
      table <- getDynamic agTable >>= traverse getDynamic
      pure (Key.fromString (show n), Aeson.object $
        [ "tolerance" .= tol, "locked" .= locked, "next-hue" .= hue ]
        ++ [ "miss-color" .= showColor miss | miss /= grey ]
        ++ [ "blend-misses" .= blendMisses | not blendMisses ]
        ++ [ "entries" .= [ Aeson.object [ "point" .= showAutocolorPoint p, "color" .= showColor c ]
                          | (p, c) <- Map.toList table ] ])
    pure (Just (Object (KM.fromList entries)))
  where
    showColor = toUserString ColorType

-- | Put a project file's @autocolor:@ key into a project's autocolor
-- state, replacing any groups it has with the same numbers.
readAutocolorState :: MonadIO io => AutocolorState -> Value -> io (Either String ())
readAutocolorState st v = liftIO $ case parseEither parseGroups v of
  Left err -> pure (Left err)
  Right groups -> Right <$> forM_ groups (\(n, (tol, locked, hue, miss, blendMisses, entries)) -> do
    g <- autocolorGroup st (AutocolorGroupId n)
    setValue (agTolerance g) tol
    setValue (agLocked g) locked
    setValue (agNextColorAngle g) hue
    setValue (agMissColor g) miss
    setValue (agBlendMisses g) blendMisses
    table <- Map.fromList <$> traverse (\(p, c) -> (p,) <$> newVariable c) entries
    setValue (agTable g) table) <* publishAutocolor st
  where
    parseGroups = withObject "autocolor" $ \o -> forM (KM.toList o) $ \(k, gv) ->
      case readMaybe (Text.unpack (Key.toText k)) of
        Nothing -> fail ("autocolor group " ++ show (Key.toText k) ++ " isn't numbered")
        Just n -> (n,) <$> parseGroup gv
    parseGroup = withObject "autocolor group" $ \o -> (,,,,,)
      <$> o .:? "tolerance" .!= 0.001
      <*> o .:? "locked" .!= False
      <*> o .:? "next-hue" .!= 0
      <*> (maybe (pure grey) (constant ColorType "color") =<< o .:? "miss-color")
      <*> o .:? "blend-misses" .!= True
      <*> (mapM parseEntry =<< o .:? "entries" .!= [])
    parseEntry = withObject "autocolor entry" $ \o -> do
      -- (a real point can be written as a plain number, which YAML reads
      -- as a number rather than text)
      x :+ y <- o Aeson..: "point" >>= \case
        Number n -> pure (realToFrac n :+ 0)
        String t -> constant ComplexType "point" (Text.unpack t)
        _ -> fail "an autocolor entry's point should be a complex number"
      c <- constant ColorType "color" =<< o Aeson..: "color"
      pure ((x, y), c)
    constant :: TypeProxy t -> String -> String -> Aeson.Parser (HaskellType t)
    constant ty what txt = case parseConstant ty txt of
      Right value -> pure value
      Left _ -> fail ("can't read the " ++ what ++ " " ++ show txt)
