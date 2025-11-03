{-# options_ghc -Wno-unused-top-binds -Wno-unused-imports #-}
module UI.ConfigEditor
  ( editConfig
  ) where

import FractalStream.Prelude hiding (get)

import Graphics.UI.WX hiding (pt, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color, parent, item, label, area)
import qualified Graphics.UI.WXCore.Events as WX
import Graphics.UI.WXCore.WxcClasses
import qualified Graphics.UI.WX as WX
import Data.IORef
import qualified Data.Map as Map
import Data.List (unsnoc, sortOn)
import Control.Concurrent.MVar
import Data.Ord (Down(..))
import Control.Monad.Trans.Maybe

import Actor.Layout
import Data.Color (Color, rgbToColor, colorToRGB)
import qualified Data.Color as Color
import Language.Type

data LayoutPart f l
  = VerticalLP [LayoutPartId]
  | HorizontalLP [LayoutPartId]
  | PanelLP l [LayoutPartId] -- implicit vertical layout
--  | TabbedLP [(l, [LayoutPartId])] l [LayoutPartId] [(l, [LayoutPartId])]
  | TextBoxLP l (f String)
  | CheckBoxLP l (f Bool)
  | ColorPickerLP l (f Color)

newtype LayoutPartId = LayoutPartId Int
  deriving (Eq, Ord, Show)

data LayoutParts f l = LayoutParts
  { getNextId    :: IO LayoutPartId
  , layoutParts  :: Map LayoutPartId (LayoutPart f l)
  , layoutParent :: Map LayoutPartId LayoutPartId
  , rootLP       :: LayoutPartId
  }

toLayout :: forall l f g
          . (l -> Label)
         -> (forall t. f t -> g t)
         -> LayoutParts f l
         -> Maybe (Layout g)
toLayout label f LayoutParts{..} = go rootLP
  where
    label' x = case label x of Label x' -> x'
    go iD = Map.lookup iD layoutParts >>= \case
      VerticalLP xs -> Vertical <$> mapM go xs
      HorizontalLP xs -> Horizontal <$> mapM go xs
      {-
      TabbedLP xs l x ys -> fmap (Tabbed . map (\l,x) -> (l, Vertical x)) $
        ((\as b cs -> as ++ [b] ++ cs)
         <$> mapM (\(l',x') -> (label' l',) <$> go x') xs
         <*> ((label' l,) <$> go x)
         <*> mapM (\(l',y') -> (label' l',) <$> go y') ys)
-}
      PanelLP n xs -> Panel (label' n) . Vertical <$> mapM go xs
      TextBoxLP n x -> pure $ TextBox (label n) (f x)
      CheckBoxLP n x -> pure $ CheckBox (label n) (f x)
      ColorPickerLP n x -> pure $ ColorPicker (label n) (f x)

insertBefore :: LayoutPartId
             -> LayoutPartId
             -> LayoutPart f l
             -> LayoutParts f l
             -> MaybeT IO (LayoutParts f l)
insertBefore tgtId srcId src lp = do
  lift $ putStrLn ("insertBefore " ++ show tgtId ++ " " ++ show srcId)
  parentID <- hoistMaybe $ Map.lookup tgtId (layoutParent lp)
  parent <- hoistMaybe $ Map.lookup parentID (layoutParts lp)
  let go xs = let lhs = takeWhile (/= tgtId) xs
                  rhs = drop (length lhs) xs
              in lhs ++ [srcId] ++ rhs
  case parent of

    VerticalLP xs -> do
      let layoutParts' = Map.insert srcId src
                       . Map.insert parentID (VerticalLP $ go xs)
                       $ layoutParts lp
          layoutParent' = Map.insert srcId parentID (layoutParent lp)
      pure lp { layoutParts = layoutParts'
              , layoutParent = layoutParent' }

    HorizontalLP xs -> do
      -- make a new vertical layout
      gpID <- lift $ getNextId lp
      origGpId <- hoistMaybe $ Map.lookup parentID (layoutParent lp)
      let layoutParts' = Map.insert gpID (VerticalLP [srcId, parentID])
                       . Map.insert parentID (HorizontalLP xs)
                       . Map.insert srcId src
                       $ layoutParts lp
          layoutParent' = Map.insert gpID origGpId
                        . Map.insert parentID gpID
                        . Map.insert srcId gpID
                        $ layoutParent lp
      pure lp { layoutParts = layoutParts'
              , layoutParent = layoutParent' }

    PanelLP n xs -> do
      let layoutParts' = Map.insert srcId src
                       . Map.insert parentID (PanelLP n $ go xs)
                       $ layoutParts lp
          layoutParent' = Map.insert srcId parentID (layoutParent lp)
      pure lp { layoutParts = layoutParts'
              , layoutParent = layoutParent' }
    _ -> fail "not a container"

insertAfter :: LayoutPartId
            -> LayoutPartId
            -> LayoutPart f l
            -> LayoutParts f l
            -> MaybeT IO (LayoutParts f l)
insertAfter tgtId srcId src lp = do
  lift $ putStrLn ("insertAfter " ++ show tgtId ++ " " ++ show srcId)
  parentID <- hoistMaybe $ Map.lookup tgtId (layoutParent lp)
  parent <- hoistMaybe $ Map.lookup parentID (layoutParts lp)
  let go xs = let lhs = takeWhile (/= tgtId) xs
                  rhs = drop (length lhs + 1) xs
              in lhs ++ [tgtId, srcId] ++ rhs
  case parent of

    VerticalLP xs -> do
      let layoutParts' = Map.insert srcId src
                       . Map.insert parentID (VerticalLP $ go xs)
                       $ layoutParts lp
          layoutParent' = Map.insert srcId parentID (layoutParent lp)
      pure lp { layoutParts = layoutParts'
              , layoutParent = layoutParent' }

    HorizontalLP xs -> do
      -- make a new vertical layout
      gpID <- lift $ getNextId lp
      origGpId <- hoistMaybe $ Map.lookup parentID (layoutParent lp)
      let layoutParts' = Map.insert gpID (VerticalLP [parentID, srcId])
                       . Map.insert parentID (HorizontalLP xs)
                       . Map.insert srcId src
                       $ layoutParts lp
          layoutParent' = Map.insert gpID origGpId
                        . Map.insert parentID gpID
                        . Map.insert srcId gpID
                        $ layoutParent lp
      pure lp { layoutParts = layoutParts'
              , layoutParent = layoutParent' }

    PanelLP n xs -> do
      let layoutParts' = Map.insert srcId src
                       . Map.insert parentID (PanelLP n $ go xs)
                       $ layoutParts lp
          layoutParent' = Map.insert srcId parentID (layoutParent lp)
      pure lp { layoutParts = layoutParts'
              , layoutParent = layoutParent' }
    _ -> fail "not a container"

insertLeft :: LayoutPartId
           -> LayoutPartId
           -> LayoutPart f l
           -> LayoutParts f l
           -> MaybeT IO (LayoutParts f l)
insertLeft tgtId srcId src lp = do
  lift $ putStrLn ("insertLeft " ++ show tgtId ++ " " ++ show srcId)
  parentID <- hoistMaybe $ Map.lookup tgtId (layoutParent lp)
  parent <- hoistMaybe $ Map.lookup parentID (layoutParts lp)
  let go xs = let lhs = takeWhile (/= tgtId) xs
                  rhs = drop (length lhs + 1) xs
              in lhs ++ [srcId, tgtId] ++ rhs
  case parent of
    HorizontalLP xs -> do
      let layoutParts' = Map.insert srcId src
                       . Map.insert parentID (HorizontalLP (go xs))
                       $ layoutParts lp
          layoutParent' = Map.insert srcId parentID (layoutParent lp)
      pure lp { layoutParts = layoutParts'
              , layoutParent = layoutParent' }

    VerticalLP xs -> do
      rowId <- lift $ getNextId lp
      let hrow = HorizontalLP [srcId, tgtId]
          lhs = takeWhile (/= tgtId) xs
          rhs = drop (length lhs + 1) xs
          xs' = lhs ++ [rowId] ++ rhs
          layoutParts' = Map.insert srcId src
                       . Map.insert rowId hrow
                       . Map.insert parentID (VerticalLP xs')
                       $ layoutParts lp
          layoutParent' = Map.insert srcId rowId
                        . Map.insert tgtId rowId
                        . Map.insert rowId parentID
                        $ layoutParent lp
      pure lp { layoutParts = layoutParts'
              , layoutParent = layoutParent' }

    PanelLP n xs -> do
      rowId <- lift $ getNextId lp
      let hrow = HorizontalLP [srcId, tgtId]
          lhs = takeWhile (/= tgtId) xs
          rhs = drop (length lhs + 1) xs
          xs' = lhs ++ [rowId] ++ rhs
          layoutParts' = Map.insert srcId src
                       . Map.insert rowId hrow
                       . Map.insert parentID (PanelLP n xs')
                       $ layoutParts lp
          layoutParent' = Map.insert srcId rowId
                        . Map.insert tgtId rowId
                        . Map.insert rowId parentID
                        $ layoutParent lp
      pure lp { layoutParts = layoutParts'
              , layoutParent = layoutParent' }

    _ -> fail "not a container"

insertRight :: LayoutPartId
            -> LayoutPartId
            -> LayoutPart f l
            -> LayoutParts f l
            -> MaybeT IO (LayoutParts f l)
insertRight tgtId srcId src lp = do
  lift $ putStrLn ("insertRight " ++ show tgtId ++ " " ++ show srcId)
  parentID <- hoistMaybe $ Map.lookup tgtId (layoutParent lp)
  parent <- hoistMaybe $ Map.lookup parentID (layoutParts lp)
  let go xs = let lhs = takeWhile (/= tgtId) xs
                  rhs = drop (length lhs + 1) xs
              in lhs ++ [tgtId, srcId] ++ rhs
  case parent of
    HorizontalLP xs -> do
      let layoutParts' = Map.insert srcId src
                       . Map.insert parentID (HorizontalLP (go xs))
                       $ layoutParts lp
          layoutParent' = Map.insert srcId parentID (layoutParent lp)
      pure lp { layoutParts = layoutParts'
              , layoutParent = layoutParent' }

    VerticalLP xs -> do
      lift $ putStrLn "here"
      rowId <- lift $ getNextId lp
      let hrow = HorizontalLP [tgtId, srcId]
          lhs = takeWhile (/= tgtId) xs
          rhs = drop (length lhs + 1) xs
          xs' = lhs ++ [rowId] ++ rhs
          layoutParts' = Map.insert srcId src
                       . Map.insert rowId hrow
                       . Map.insert parentID (VerticalLP xs')
                       $ layoutParts lp
          layoutParent' = Map.insert srcId rowId
                        . Map.insert tgtId rowId
                        . Map.insert rowId parentID
                        $ layoutParent lp
      pure lp { layoutParts = layoutParts'
              , layoutParent = layoutParent' }

    PanelLP n xs -> do
      rowId <- lift $ getNextId lp
      let hrow = HorizontalLP [tgtId, srcId]
          lhs = takeWhile (/= tgtId) xs
          rhs = drop (length lhs + 1) xs
          xs' = lhs ++ [rowId] ++ rhs
          layoutParts' = Map.insert srcId src
                       . Map.insert rowId hrow
                       . Map.insert parentID (PanelLP n xs')
                       $ layoutParts lp
          layoutParent' = Map.insert srcId rowId
                        . Map.insert tgtId rowId
                        . Map.insert rowId parentID
                        $ layoutParent lp
      pure lp { layoutParts = layoutParts'
              , layoutParent = layoutParent' }

    _ -> fail "not a container"

insertAt :: Position
         -> LayoutPartId
         -> LayoutPartId
         -> LayoutPart f l
         -> LayoutParts f l
         -> MaybeT IO (LayoutParts f l)
insertAt = \case
  North -> insertBefore
  South -> insertAfter
  East  -> insertRight
  West  -> insertLeft

-- | Unhook an item from its parent, returning the item and the
-- new layout.
orphanItem :: LayoutPartId
           -> LayoutParts f l
           -> Maybe (LayoutPart f l, LayoutParts f l)
orphanItem iD lp = do
  parentID <- Map.lookup iD (layoutParent lp)
  parent <- Map.lookup parentID (layoutParts lp)
  parent' <- case parent of
        VerticalLP xs   -> pure $ VerticalLP (filter (/= iD) xs)
        HorizontalLP xs -> pure $ HorizontalLP (filter (/= iD) xs)
        PanelLP n xs    -> pure $ PanelLP n (filter (/= iD) xs)
{-
        TabbedLP xs l a ys ->
          let xs' = filter ((/= iD) . snd) xs
              ys' = filter ((/= iD) . snd) ys
              a'  = if a == iD then HorizontalLP [] else a
          in TabbedLP xs' l a' ys'
-}
        _ -> Nothing -- other layouts should not be a parent
  let layoutParent' = Map.delete iD (layoutParent lp)
      layoutParts' = Map.delete iD
                   . Map.insert parentID parent'
                   $ layoutParts lp
  item <- Map.lookup iD (layoutParts lp)
  pure (item, lp { layoutParent = layoutParent'
                 , layoutParts  = layoutParts' })

data Position = North | South | East | West

positionFrom :: (Point, Size) -> (Point, Size) -> Position
positionFrom (Point x y, Size w h) (Point x' y', Size w' h') =
  let (cx,  cy)  = (x + w `div` 2, y + h `div` 2)
      (cx', cy') = (x' + w' `div` 2, y' + h' `div` 2)
      (dx, dy) = (cx' - cx, cy' - cy)
  in if abs dx > 3 * abs dy
     then (if dx > 0 then East else West)
     else (if dy < 0 then North else South)

editConfig :: IO ()
editConfig = do


  f <- frame [ text := "Edit configuration"
             , on resize := propagateEvent
             ]
  p <- panel f []
  innerP <- panel p []

  WX.windowOnClose f (set f [ visible := False ])

  initialLP <- newLayoutParts

  offThreadLayoutChange <- newEmptyMVar
  currentLayout <- newMVar initialLP

  widgetDimensions <- newIORef Map.empty

  let editLayout :: (LayoutParts SB String -> IO (Maybe (LayoutParts SB String)))
                 -> IO ()
      editLayout edit = modifyMVar_ currentLayout $ \lo -> edit lo >>= \case
        Nothing  -> putStrLn "no layout change" >> pure lo
        Just lo' -> do
          putStrLn "setting new layout"
          putMVar offThreadLayoutChange (toLO editLayout registerSize finishDrag lo')
          putStrLn "  ok"
          pure lo'

      registerSize :: LayoutPartId -> IO (Point, Size) -> IO ()
      registerSize i getter = modifyIORef widgetDimensions (Map.insert i getter)

      moveTo :: Position -> LayoutPartId -> LayoutPartId -> IO ()
      moveTo pos src tgt = editLayout $ \lp -> runMaybeT $ do
        (item, lp') <- hoistMaybe $ orphanItem src lp
        insertAt pos tgt src item lp'

      moveInto :: LayoutPartId -> LayoutPartId -> IO ()
      moveInto src tgt = editLayout $ \lp -> runMaybeT $ do
        (item, lp') <- hoistMaybe $ orphanItem src lp
        target <- hoistMaybe $ Map.lookup tgt (layoutParts lp')
        case target of
          PanelLP name xs -> do
            let target' = PanelLP name (src : xs)
            pure lp'
              { layoutParts = Map.insert src item
                            . Map.insert tgt target'
                            $ layoutParts lp'
              , layoutParent = Map.insert src tgt
                             $ layoutParent lp'
              }
          _ -> fail "cannot move into a non-panel"

      finishDrag :: LayoutPartId -> Point -> Size -> IO ()
      finishDrag i dp ds = do
        dims <- readIORef widgetDimensions >>= sequence
        putStrLn ("Finish drag: " ++ show i ++ ", " ++ show dp ++ ", " ++ show ds)
        putStrLn ("  dims=" ++ show dims)
        let hits = sortOn (\(_, (r, _, _)) -> Down (area r))
                 . concatMap (\(k,v) -> case v of { Nothing -> []; Just x -> [(k,x)]})
                 . Map.toList
                 . fmap (overlap (dp, ds))
                 $ dims
        putStrLn ("  hits=" ++ show hits)
        let pickBest = \case
              ((j, (r, _, yContained)): etc) -> do
                -- If j is an empty Panel and yContained is true, then move into the panel
                lo <- readMVar currentLayout
                let pos = positionFrom r (dp, ds)
                case layoutParts lo Map.! j of
                  PanelLP _ []
                    | yContained -> moveInto i j
                    | otherwise  -> moveTo pos i j
                  PanelLP _ _ -> pickBest etc
                  _ -> moveTo pos i j
              _ -> editLayout (pure . Just) -- trigger a refresh to put this widget back
        pickBest hits

  addThing <- choice p [
    items := [ "Add..."
             , "Numeric variable"
             , "Boolean variable"
             , "Color variable"
             , "Grouping"
             ]
    ]
  ok <- button p [ text := "OK" ]
  cancel <- button p [ text := "Cancel" ]

  set addThing [
    on select := do
        ix <- get addThing selection
        case ix of
          1 -> editLayout (newTextBox' "label" (S "initial value" "" ComplexT))
          2 -> editLayout (newCheckBox' "label" (B False ""))
          3 -> editLayout (newColorPicker' "label" (C Color.red ""))
          4 -> editLayout (newPanel' "label")
          _ -> pure ()
        set addThing [ selection := 0 ]
    ]


  set f [ layout := container p $ glue
        , position := Point 100 100 ]

  -- Add a timer that checks for layout changes from off the main UI
  -- thread, ~20Hz
  let triggerRepaint = do
        isVisible <- get f visible
        when isVisible $ do
          windowReFit f
          --windowLayout f
          --refit f --windowFit f
          repaint p
          windowRefresh p True -- True=redraw background
          windowUpdateWindowUI p
  _ <- timer f [ interval := 50
               , enabled := True
               , on command := do
                   mlo <- tryTakeMVar offThreadLayoutChange
                   whenJust mlo $ \makeInnerLayout -> do
                     windowDestroyChildren innerP
                     writeIORef widgetDimensions Map.empty
                     innerLayout <- makeInnerLayout innerP
                     --set innerP [ layout := fill $ innerLayout ]
                     set f [ layout := container p $ margin 5 $
                              fill $ column 5 $
                             [ hstretch . expand $ container innerP $ fill innerLayout
                             , hstretch  . alignBottom $
                               row 0 [ margin 3 $ widget addThing
                                     , hglue
                                     , margin 3 $ widget ok
                                     , margin 3 $ widget cancel ]
                             ]
                           ]
                     --windowReLayout f
                     triggerRepaint
               ]

  -- trigger the initial draw of the layout
  putMVar offThreadLayoutChange (toLO editLayout registerSize finishDrag initialLP)

getPosition :: Window a -> IO Point
getPosition p
  | objectIsNull p = pure (Point 0 0)
  | otherwise = do
      Point x y <- get p position
      p' <- get p WX.parent
      Point x' y' <- getPosition p'
      pure (Point (x + x') (y + y'))

getDimensions :: Panel a -> IO (Point, Size)
getDimensions p = (,) <$> getPosition p <*> get p outerSize

toLO :: ((LayoutParts SB String -> IO (Maybe (LayoutParts SB String))) -> IO ())
     -> (LayoutPartId -> IO (Point, Size) -> IO ())
     -> (LayoutPartId -> Point -> Size -> IO ())
     -> LayoutParts SB String
     -> Window a
     -> IO WX.Layout
toLO editLayout registerSize finishDrag lp0 p0 = go p0 (rootLP lp0)
  where
    popEdit = editVariable p0 editLayout

    go :: forall b
        . Window b
       -> LayoutPartId
       -> IO WX.Layout
    go p iD = case layoutParts lp0 Map.! iD of

      VerticalLP parts -> do
        p' <- panel p []
        lo <- if null parts
              then pure glue
              else fill . column 5 <$> mapM (go p') parts
        set p' [ layout := lo ]
        pure (fill $ widget p')

      HorizontalLP parts -> do
        p' <- panel p []
        lo <- if null parts
              then pure glue
              else fill . margin 10 . row 5 <$> mapM (go p') parts
        set p' [ layout := lo ]
        pure (fill $ widget p')

      PanelLP name parts -> do
        r <- panel p []
        p' <- panel r []
        lo <- if null parts
              then pure glue
              else fill . column 5 <$> mapM (go p') parts
        set p' [ layout := lo ]
        registerSize iD (getDimensions r)
        addMouseHandlers (finishDrag iD) r
        set r [ on clickRight := \_pt ->
                  popEdit "Configuration group" iD $ \d -> do

                    te <- textEntry d [ text := name
                                      , processEnter := True
                                      ]
                    let innerLO = [ row 5 [ WX.label "Group name:"
                                          , hstretch $ expand $ widget te ]]
                        action = do
                          newLabel <- get te text
                          pure (PanelLP newLabel parts)
                    pure (innerLO, action)

              ]
        pure (fill $ container r $ boxed name $ fill $ widget p')

      CheckBoxLP name (B b v) -> do
        r <- panel p []
        registerSize iD (getDimensions r)
        cb <- checkBox r [ text := name
                         , checkable := True
                         , checked := b
                         , visible := True
                         , enabled := True ]
        addMouseHandlers (finishDrag iD) r
        set r [ on clickRight := \_pt -> popEdit "Boolean variable" iD $ \d -> do
                  te1 <- textEntry d [ text := name
                                     , processEnter := True
                                     ]
                  te2 <- textEntry d [ text := v
                                     , processEnter := True
                                     ]
                  b' <- get cb checked
                  cb' <- checkBox d [ text := "Initially checked?"
                                    , checkable := True
                                    , checked := b' ]
                  let lo = [ row 5 [ WX.label "Label text:"
                                   , hstretch $ expand $ widget te1 ]
                           , row 5 [ WX.label "Variable name:"
                                   , hstretch $ expand $ widget te2 ]
                           , row 5 [ widget cb', hglue ] ]
                      action = do
                        newLabel <- get te1 text
                        newVar <- get te2 text
                        b'' <- get cb' checked
                        pure (CheckBoxLP newLabel (B b'' newVar))

                  pure (lo, action)
              ]

        pure (hfill $ container r $ hstretch (widget cb))

      TextBoxLP name (S c v t) -> do
        r <- panel p []
        registerSize iD (getDimensions r)
        te <- textEntry r [ text := c
                          , processEnter := True
                          , tooltip := "" ]

        addMouseHandlers (finishDrag iD) r
        set r [ on clickRight := \_ -> popEdit "Numeric variable" iD $ \d -> do
                  te1 <- textEntry d [ text := name
                                     , processEnter := True
                                     ]
                  te2 <- textEntry d [ text := v
                                     , processEnter := True
                                     ]
                  c' <- get te text
                  te3 <- textEntry d [ text := c'
                                     , processEnter := True
                                     ]
                  ch <- choice d [ items := [ "ℂ", "ℝ", "ℤ" ] ]
                  case t of
                      ComplexT -> set ch [ selection := 0 ]
                      RealT    -> set ch [ selection := 1 ]
                      IntegerT -> set ch [ selection := 2 ]
                      _ -> set ch [ selection := 0 ]

                  let lo = [ row 5 [ WX.label "Label text:"
                                   , hstretch $ expand $ widget te1 ]
                           , row 5 [ WX.label "Variable name:"
                                   , hstretch $ expand $ widget te2
                                   , WX.label "Variable type:"
                                   , hstretch $ widget ch ]
                           , row 5 [ WX.label "Initial value:"
                                   , hstretch $ expand $ widget te3 ]]

                  let ok = do
                        newLabel <- get te1 text
                        newVar <- get te2 text
                        newValue <- get te3 text
                        newTy <- get ch selection <&> \case
                            0 -> ComplexT
                            1 -> RealT
                            2 -> IntegerT
                            _ -> error "unreachable"
                        pure (TextBoxLP newLabel (S newValue newVar newTy))

                  pure (lo, ok)
              ]

        pure (hfill $ container r $ hfill $
              row 5 [ margin 3 (WX.label name)
                    , hstretch (widget te)])

      ColorPickerLP name (C c v) -> do
        r <- panel p []
        registerSize iD (getDimensions r)
        let (r0, g0, b0) = colorToRGB c
        picker <- feed2 [ text := name, visible := True ] 0 $
                  initialWindow $ \i rect' ps s -> do
                    e <- colorPickerCtrlCreate r i (rgb r0 g0 b0) rect' s
                    set e ps
                    pure e
        addMouseHandlers (finishDrag iD) r
        set r [ on clickRight := \_ -> popEdit "Color variable" iD $ \d -> do

                  te1 <- textEntry d [ text := name
                                     , processEnter := True
                                     ]
                  te2 <- textEntry d [ text := v
                                     , processEnter := True
                                     ]
                  c' <- colorPickerCtrlGetColour picker
                  let (r1 :: Word8, g1, b1) = (colorRed c', colorGreen c', colorBlue c')
                  picker' <- feed2 [ text := "Initial color", visible := True ] 0 $
                             initialWindow $ \i rect' ps s -> do
                               e <- colorPickerCtrlCreate d i
                                      (rgb r1 g1 b1) rect' s
                               set e ps
                               pure e

                  let lo = [ row 5 [ WX.label "Label text:"
                                   , hstretch $ widget te1 ]
                           , row 5 [ WX.label "Variable name:"
                                   , hstretch $ widget te2 ]
                           , hstretch $ widget picker' ]
                      ok = do
                        newLabel <- get te1 text
                        newVar <- get te2 text
                        c'' <- colorPickerCtrlGetColour picker'
                        let r2 = colorRed c''
                            g2 = colorGreen c''
                            b2 = colorBlue c''
                            newC = rgbToColor (r2, g2, b2)
                        pure (ColorPickerLP newLabel (C newC newVar))

                  pure (lo, ok)
                  ]


        pure (hfill $ container r $ hfill $
              row 5 [ margin 3 (WX.label name)
                    , hstretch (widget picker)])

newLayoutParts :: IO (LayoutParts f l)
newLayoutParts = do
  nextId <- newMVar (0 :: Int)
  let getNextId = do
        n <- takeMVar nextId
        putMVar nextId (n + 1)
        pure (LayoutPartId n)
  rootLP <- getNextId
  let layoutParts = Map.singleton rootLP (VerticalLP [])
      layoutParent = Map.empty
  pure LayoutParts{..}

newTextBox :: l
           -> f String
           -> StateT (LayoutParts f l) IO ()
newTextBox l t = addRootChild (TextBoxLP l t)

newTextBox' :: String
            -> SB String
            -> LayoutParts SB String
            -> IO (Maybe (LayoutParts SB String))
newTextBox' l t lo = do
  lo' <- execStateT (newTextBox l t) lo
  pure (Just lo')

newCheckBox :: l
            -> f Bool
            -> StateT (LayoutParts f l) IO ()
newCheckBox l t = addRootChild (CheckBoxLP l t)

newCheckBox' :: String
             -> SB Bool
             -> LayoutParts SB String
             -> IO (Maybe (LayoutParts SB String))
newCheckBox' l t lo = do
  lo' <- execStateT (newCheckBox l t) lo
  pure (Just lo')


newColorPicker :: l
               -> f Color
               -> StateT (LayoutParts f l) IO ()
newColorPicker l t = addRootChild (ColorPickerLP l t)

newColorPicker' :: String
                -> SB Color
                -> LayoutParts SB String
                -> IO (Maybe (LayoutParts SB String))
newColorPicker' l t lo = do
  lo' <- execStateT (newColorPicker l t) lo
  pure (Just lo')


newPanel :: l -> StateT (LayoutParts f l) IO ()
newPanel l = addRootChild (PanelLP l [])

newPanel' :: String
          -> LayoutParts SB String
          -> IO (Maybe (LayoutParts SB String))
newPanel' l lo = do
  lo' <- execStateT (newPanel l) lo
  pure (Just lo')

addRootChild :: LayoutPart f l
             -> StateT (LayoutParts f l) IO ()
addRootChild part = do
  lps <- gets id
  iD <- lift $ getNextId lps
  rootId <- gets rootLP
  root <- gets ((Map.! rootId) . layoutParts)
  root' <- case root of
    VerticalLP xs -> pure (VerticalLP (xs ++ [iD]))
    _ -> do
      newRoot <- lift $ getNextId lps
      modify' (\s -> s { rootLP = newRoot
                       , layoutParent = Map.insert rootId newRoot
                                      $ layoutParent s })
      pure (VerticalLP [rootId, iD])

  modify' (\s -> s { layoutParts = Map.insert iD part
                                 . Map.insert (rootLP s) root'
                                 $ layoutParts s
                   , layoutParent = Map.insert iD (rootLP s) (layoutParent s)
                   })

data SB t where
  S :: String -> String -> FSType -> SB String
  B :: Bool -> String -> SB Bool
  C :: Color -> String -> SB Color

deriving instance Eq (SB t)
deriving instance Ord (SB t)
deriving instance Show (SB t)


editVariable :: forall t
               . Window t
              -> ((LayoutParts SB String -> IO (Maybe (LayoutParts SB String))) -> IO ())
              -> String
              -> LayoutPartId
              -> (forall a. Panel a -> IO ([WX.Layout], IO (LayoutPart SB String)))
              -> IO ()
editVariable w editLayout title iD action = do
  d <- dialog w [ text := title ]
  dp <- panel d []
  ok <- button dp [ text := "OK" ]
  cancel <- button dp [ text := "Cancel" ]
  delete <- button dp [ text := "Delete" ]
  (lo, cont) <- action dp
  set d [ layout := container dp $ margin 10 $ fill $
                   margin 5 $ column 5
                   (lo ++ [row 5 [ widget ok, widget cancel, hglue, widget delete ]])]

  result <- showModal d $ \k -> do
    set ok [ on command := cont >>= (k . Just . Just) ]
    set cancel [ on command := k Nothing ]
    set delete [ on command := k (Just Nothing) ]
  case result of
    Nothing -> pure ()
    Just Nothing -> editLayout $ \lo' -> pure . fmap snd $ orphanItem iD lo'
    Just (Just part) -> editLayout $ \lo' ->
      pure . Just $ lo' { layoutParts = Map.insert iD part (layoutParts lo') }

addMouseHandlers :: forall t
                  . (Point -> Size -> IO ())
                 -> Panel t
                 -> IO ()
addMouseHandlers finishDrag p = do
  origBg <- get p bgcolor >>= newIORef
  dragPt <- newIORef Nothing
  set p [
    on mouse := \case

        MouseLeftDown (Point x y) modifiers | isNoShiftAltControlDown modifiers -> do
          writeIORef dragPt (Just (x,y))
          get p bgcolor >>= writeIORef origBg
          set p [ bgcolor := colorSystem ColorHighlight ]
          raiseRecursively p
          windowRefresh p True

        MouseLeftUp _pt modifiers | isNoShiftAltControlDown modifiers -> do
          writeIORef dragPt Nothing
          bg <- readIORef origBg
          set p [ bgcolor := bg ]
          windowRefresh p True
          getDimensions p >>= uncurry finishDrag

        MouseLeftDrag (Point x y) modifiers | isNoShiftAltControlDown modifiers -> do
          orig <- readIORef dragPt
          whenJust orig $ \(x0, y0) -> do
            let (dx, dy) = (x - x0, y - y0)
            when (dx /= 0 || dy /= 0) $ do
              Point posX posY <- get p position
              set p [ position := Point (posX + dx) (posY + dy) ]

        _ -> propagateEvent
    ]

overlap :: (Point, Size) -> (Point, Size) -> Maybe ((Point, Size), Bool, Bool)
overlap (Point x0 y0, Size w0 h0) (Point x1 y1, Size w1 h1) =
  let xOverlap = (x0 > x1 && x0 < x1 + w1) || (x1 > x0 && x1 < x0 + w0)
      yOverlap = (y0 > y1 && y0 < y1 + h1) || (y1 > y0 && y1 < y0 + h0)
      xContained = (x0 > x1 && x0 + w0 < x1 + w1) || (x1 > x0 && x1 + w1 > x0 + w0)
      yContained = (y0 > y1 && y0 + h0 < y1 + h1) || (y1 > y0 && y1 + h1 > y0 + h0)
  in if xOverlap && yOverlap
     then let x = max x0 x1
              y = max y0 y1
              w = min (x0 + w0) (x1 + w1) - x
              h = min (y0 + h0) (y1 + h1) - y
          in Just ((Point x y, Size w h), xContained, yContained)
     else Nothing

area :: (Point, Size) -> Int
area (_, Size w h) = w * h

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mx = ($ mx) . maybe (pure ())

raiseRecursively :: Window a -> IO ()
raiseRecursively w = do
  p <- get w WX.parent
  unless (objectIsNull p) (raiseRecursively p)
  windowRaise w
