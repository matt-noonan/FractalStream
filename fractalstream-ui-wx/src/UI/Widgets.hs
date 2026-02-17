module UI.Widgets
  ( wxTimer
  , wxWatchDynamic1
  , wxWatchDynamic
  , checkboxWidget
  , selectionWidget
  , plainTextWidget
  , buttonWidget
  , colorWidget
  , expressionWidget
  ) where

import FractalStream.Prelude hiding (get)

import Language.Type
import Data.DynamicValue
import Data.Color hiding (black, white)
import Actor.Layout

import UI.CodeEditor

import Graphics.UI.WX hiding (pt, when, grey, Color)
import Graphics.UI.WXCore.Events
import Graphics.UI.WXCore.WxcClassesAL
import Graphics.UI.WXCore.WxcClassesMZ
import Graphics.UI.WXCore.WxcClassTypes (ColourPickerCtrl)
import Control.Concurrent
import Data.Char (toUpper)
import Text.Printf

simpleDialog :: (MonadIO io, Widget (Window w), Eq t)
             => String
             -> Maybe String
             -> (Window w -> IO t)
             -> (forall b. Window b -> t -> IO (Window w))
             -> Window a
             -> Variable t
             -> io ()
simpleDialog title m'label onOk makeControl p v = liftIO $ do
  editDialog <- dialog p [ text := title, resizeable := True ]
  ep <- panel editDialog []
  v0 <- getDynamic v
  control <- makeControl ep v0
  let widgetRow = case m'label of
        Nothing -> fill (widget control)
        Just l  -> row 5 [ label l, fill (widget control) ]

  epOk <- button ep [ text := "Ok" ]
  epCancel <- button ep [ text := "Cancel" ]
  set editDialog [ layout := margin 10 $ fill $ container ep $ column 5
                 [ widgetRow
                 , row 5 [ margin 3 $ widget epCancel, hglue, margin 3 $ widget epOk ]]]
  windowSetFocus control
  change <- fmap isJust $ showModal editDialog $ \endEdit -> do
    set epOk [ on command := endEdit (Just ()) ]
    set epCancel [ on command := endEdit Nothing ]
  when change $ liftIO (onOk control >>= setValue v)
  void (windowClose editDialog True)


editLabel :: MonadIO io => Window a -> Variable Label -> io ()
editLabel = simpleDialog "Edit label" (Just "Label: ")
  (\w -> Label <$> get w text)
  (\ep (Label lab) -> textEntry ep [ text := lab ])

editString :: MonadIO io => String -> Window a -> Variable String -> io ()
editString s = simpleDialog ("Edit " ++ s) (Just $ uppercase s ++ ": ")
  (\w -> get w text)
  (\ep var -> textEntry ep [ text := var ])
 where
   uppercase = \case
     (c:cs) -> toUpper c : cs
     ""     -> ""

editMultilineText :: MonadIO io => Window a -> Variable String -> io ()
editMultilineText = simpleDialog "Edit text" Nothing
  (\w -> get w text)
  (\ep txt -> textCtrl ep [ text := txt ])

buttonWidget :: MonadIO io => Window a -> Variable String -> io (Button ())
buttonWidget p s = liftIO $ do
  txt0 <- getDynamic s
  btn <- button p [ text := txt0 ]
  editMenu <- menuPane [ text := "" ]
  menuItem editMenu [ text := "Edit button label...", on command := editString "button label" btn s ]
  set btn [ on clickRight := \pt -> menuPopup editMenu pt btn ]
  wxWatchDynamic1 p s $ \newText -> set btn [ text := newText ]
  pure btn

checkboxWidget :: MonadIO io => Window a -> Variable Label -> Parsed String -> Variable Bool -> io (CheckBox ())
checkboxWidget p l v b = liftIO $ do
  Label lab0 <- getDynamic l
  initial <- getDynamic b

  cb <- checkBox p [ text := lab0, checkable := True, checked := initial ]

  editMenu <- menuPane [ text := "" ]
  var <- getDynamic (source v)
  info <- menuItem editMenu [ text := var ++ " : Boolean" ]
  wxWatchDynamic1 cb (source v) $ \newVar -> set info [ text := newVar ++ " : Boolean" ]
  menuLine editMenu
  menuItem editMenu [ text := "Edit label...", on command := editLabel cb l ]
  menuItem editMenu [ text := "Edit variable...", on command := editString "variable name" cb (source v) ]

  set cb [ on command := do
             isChecked <- get cb checked
             void (setValue b isChecked)
         , on clickRight := \pt -> menuPopup editMenu pt cb ]

  wxWatchDynamic1 cb b (\isChecked -> set cb [ checked := isChecked ])
  wxWatchDynamic1 cb l (\(Label lab) -> set cb [ text := lab ])
  pure cb

selectionWidget :: MonadIO io => Window a -> Parsed String -> Variable Int64 -> Variable [String] -> io (Choice ())
selectionWidget p v pick options = liftIO $ do
  ix0' <- getDynamic pick
  opts0 <- getDynamic options
  let ix0 = if fromIntegral ix0' >= length opts0 then 0 else ix0'
  c <- choice p [ selection := fromIntegral ix0, items := opts0 ]

  editMenu <- menuPane [ text := "" ]
  var <- getDynamic (source v)
  info <- menuItem editMenu [ text := var ++ " : ℤ" ]
  wxWatchDynamic1 c (source v) $ \newVar -> set info [ text := newVar ++ " : ℤ" ]
  menuLine editMenu
  menuItem editMenu [ text := "Edit variable...", on command := editString "variable name" c (source v) ]

  set c [ on select := do
            newIx <- fromIntegral <$> get c selection
            setValue pick newIx
        , on clickRight := \pt -> menuPopup editMenu pt c ]

  wxWatchDynamic1 c pick $ \newIx -> do
    numOpts <- length <$> get c items
    if fromIntegral newIx >= numOpts
      then set c [ enabled := False ]
      else set c [ enabled := True, selection := fromIntegral newIx ]
  pure c

plainTextWidget :: MonadIO io => Window a -> Variable String -> io (StaticText ())
plainTextWidget p s = liftIO $ do
  txt <- staticText p [ text := "" ]
  editMenu <- menuPane [ text := "" ]
  menuItem editMenu [ text := "Edit text...", on command := editMultilineText txt s ]
  set txt [ on clickRight := \pt -> menuPopup editMenu pt txt ]
  wxWatchDynamic1 p s $ \newText -> set txt [ text := newText ]
  pure txt

colorWidget :: MonadIO io
            => Window a
            -> Variable Label
            -> Parsed String
            -> Parsed Color
            -> io (StaticText (), ColourPickerCtrl ())
colorWidget p l v col = liftIO $ do
  (r0, g0, b0) <- colorToRGB . fromRight grey <$> getDynamic col
  picker <- feed2 [ visible := True ] 0 $
    initialWindow $ \iD rect' ps s -> do
      e <- colorPickerCtrlCreate p iD (rgb r0 g0 b0) rect' s
      set e ps
      pure e
  let newPick = do
        c <- colorPickerCtrlGetColour picker
        let r = fromIntegral (colorRed   c :: Word8) / 255.0 :: Double
            g = fromIntegral (colorGreen c :: Word8) / 255.0 :: Double
            b = fromIntegral (colorBlue  c :: Word8) / 255.0 :: Double
        setValue (source col) (printf "rgb(%0.3f, %0.3f, %0.3f)" r g b :: String)

  wxWatchDynamic1 p col $ \case
    Left _ -> pure ()
    Right newColor -> do
      let (r,g,b) = colorToRGB newColor
      colorPickerCtrlSetColour picker (rgb r g b)
  windowOnEvent picker [wxEVT_COMMAND_COLOURPICKER_CHANGED] newPick (const newPick)

  editMenu <- menuPane [ text := "" ]
  var <- getDynamic (source v)
  info <- menuItem editMenu [ text := var ++ " : Color" ]
  wxWatchDynamic1 picker (source v) $ \newVar -> set info [ text := newVar ++ " : Color" ]
  menuLine editMenu
  menuItem editMenu [ text := "Edit label...", on command := editLabel picker l ]
  menuItem editMenu [ text := "Edit variable..."
                    , on command := editString "variable name" picker (source v) ]

  Label lab0 <- getDynamic l
  labelTxt <- liftIO $ staticText p [ text := lab0 ]
  wxWatchDynamic1 p l $ \(Label lab) -> do
    set labelTxt [ text := lab ]
    windowReLayout p

  set picker   [ on clickRight := \pt -> menuPopup editMenu pt picker ]
  set labelTxt [ on clickRight := \pt -> menuPopup editMenu pt labelTxt ]

  pure (labelTxt, picker)

expressionWidget :: MonadIO io
                 => Window a
                 -> Variable Label
                 -> UIVariable
                 -> io (StaticText (), TextCtrl (), StaticText ())
expressionWidget = genericExpressionWidget False

genericExpressionWidget :: MonadIO io
                        => Bool
                        -> Window a
                        -> Variable Label
                        -> UIVariable
                        -> io (StaticText (), TextCtrl (), StaticText ())
genericExpressionWidget _canEditEnvironment p l UIVariable{..} = liftIO $ do
  errorMessage <- staticText p [ text := "" ]

  initial <- getDynamic (source exprValue)
  te <- textEntry p [ text := initial
                    , processEnter := True
                    , tooltip := ""
                    ]
  normalBG <- get te bgcolor

  -- TODO: only refresh if the value has changed
  let setErrorMessage = \case
        Nothing -> do
          set te [ bgcolor := normalBG ]
          set errorMessage [ text := "" ]
          windowReLayout p
        Just err -> do
          set te [ bgcolor := rgb 180 80 (80 :: Int)]
          set errorMessage [ text :=  "⚠️", tooltip := err
                           , on click := \_ -> do
                               tw <- tipWindowCreate p "" 1000
                               textCol <- frameIsLight tw <&> \case
                                 True  -> black
                                 False -> white
                               _ <- staticText tw [ text := err
                                                  , font := fontFixed
                                                  , fontSize := 12
                                                  , color := textCol ]
                               windowReLayout tw
                               windowRefresh tw True
                           ]
          windowReLayout p

  set te [ on command := do
             newText <- get te text
             setValue (source exprValue) newText
             getDynamic exprValue >>= \case
               Left msg -> setErrorMessage (Just msg)
               _        -> setErrorMessage Nothing
         , on focus := \case
             True -> propagateEvent
             False -> do
               newText <- get te text
               oldText <- getDynamic (source exprValue)
               when (newText /= oldText) $ do
                 setValue (source exprValue) newText
                 getDynamic exprValue >>= \case
                   Left msg -> setErrorMessage (Just msg)
                   _        -> setErrorMessage Nothing
               propagateEvent
         ]
  wxWatchDynamic1 te (source exprValue) (\newText -> set te [ text := newText ])

  editMenu <- menuPane [ text := "" ]
  info <- menuItem editMenu [ text := "--" ]
  wxWatchDynamic1 te exprName $ \newVar -> do
    let var = fromRight "???" newVar
    ty <- either (const "???") (\(SomeType t) -> ppType t) <$> getDynamic exprType
    set info [ text := var ++ " : " ++ ty ]
  wxWatchDynamic1 te exprType $ \newTy -> do
    var <- fromRight "???" <$> getDynamic exprName
    let ty = either (const "???") (\(SomeType t) -> ppType t) newTy
    set info [ text := var ++ " : " ++ ty ]
  menuLine editMenu
  menuItem editMenu [ text := "Edit label...", on command := editLabel te l ]
  menuItem editMenu [ text := "Edit variable..."
                    , on command := editString "variable name" te (source exprName) ]
  menuItem editMenu [ text := "Edit type..."
                    , on command := editString "type" te (source exprType) ]

  labelTxt <- staticText p [ text := "" ]
  wxWatchDynamic1 p l $ \(Label lab) -> do
    set labelTxt [ text := lab ]
    windowReLayout p

  set labelTxt [ on clickRight := \pt -> menuPopup editMenu pt labelTxt ]
  set te       [ on clickRight := \pt -> menuPopup editMenu pt te ]

  pure (labelTxt, te, errorMessage)

-- | Like `watchDynamic`, but ensures that the action
-- runs on the main UI thread. Automatically attaches
-- the halt action to the closing of `p`, and runs the
-- action once on construction.
wxWatchDynamic :: (AsDynamic f, MonadIO io) => Window b -> f a -> (a -> IO ()) -> io ()
wxWatchDynamic p dv action = liftIO $ do
  todo <- newMVar []
  halt <- watchDynamic dv $ \x -> modifyMVar_ todo (\actions -> pure (action x : actions))
  _ <- wxTimer p [ interval := 100
                 , enabled := True
                 , on command := tryTakeMVar todo >>= \case
                     Nothing -> pure ()
                     Just actions -> do
                       putMVar todo []
                       sequence_ (reverse actions)
                 ]
  set p [ on closing :~ \previous -> halt >> previous ]

-- | Same as `wxWatchDynamic`, but also
wxWatchDynamic1 :: (AsDynamic f, MonadIO io) => Window b -> f a -> (a -> IO ()) -> io ()
wxWatchDynamic1 p dv action = do
  wxWatchDynamic p dv action
  -- Run the action once
  liftIO (getDynamic dv >>= action)

wxTimer :: MonadIO io => Window a -> [Prop Timer] -> io Timer
wxTimer w props = liftIO $ do
  t <- timer w props
  set w [ on closing :~ \previous -> timerStop t >> previous ]
  pure t
