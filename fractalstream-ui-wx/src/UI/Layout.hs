module UI.Layout
  ( generateWxLayout
  ) where

import FractalStream.Prelude hiding (get)

import Data.Color (colorToRGB, grey)
import Data.DynamicValue
import Actor.Layout
import Data.IORef
import Text.Printf (printf)

import UI.CodeEditor

import Graphics.UI.WX hiding (pt, grey, glue, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color)
import qualified Graphics.UI.WXCore.Events as WX
import qualified Graphics.UI.WX as WX
import           Graphics.UI.WXCore.WxcClassesAL
import           Graphics.UI.WXCore.WxcClassesMZ
import           Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.Frame (windowGetScreenPosition)

generateWxLayout :: (String -> IO ())
                 -> Window a
                 -> Layout
                 -> IO (IO (), WX.Layout)

generateWxLayout buttonPress frame0 wLayout = do
  panel0 <- panel frame0 []
  (computedLayout, done) <- runStateT (go panel0 wLayout) (pure ())
  pure (done, container panel0 $ fill $ computedLayout)

 where

   watch :: forall f a. AsDynamic f => f a -> (a -> IO ()) -> StateT (IO ()) IO ()
   watch d action = do
     done <- watchDynamic d action
     modify (>> done)

   go :: Window a -> Layout -> StateT (IO ()) IO WX.Layout
   go p = \case

     Panel pTitle innerLayout -> do
       Label title <- getDynamic pTitle
       p' <- liftIO $ panel p [ ]
       inner <- getDynamic innerLayout
       lo <- go p' inner
       liftIO $ set p' [ layout := lo ]
       pure (hstretch $ expand $ margin 5 $ boxed title (fill $ widget p'))

     Vertical v -> do
       p' <- liftIO $ panel p []
       parts <- getDynamic v
       lo <- hstretch . expand . margin 5 . column 5 <$> mapM (go p') parts
       liftIO $ set p' [ layout := lo ]
       pure (hstretch $ expand (widget p'))

     Horizontal v -> do
       p' <- liftIO $ panel p []
       parts <- getDynamic v
       lo <- fill . margin 5 . row 5 <$> mapM (go p') parts
       liftIO $ set p' [ layout := lo ]
       pure (hstretch $ expand (widget p'))

     Tabbed dts -> do
       nb <- liftIO $ feed2 [ visible := True ] 0 $
             initialWindow $ \iD rect' ps s -> do
                   e <- notebookCreate p iD rect' s
                   set e ps
                   pure e
       ts <- getDynamic dts
       forM_ ts $ \TabItem{..} -> do
         Label lab <- getDynamic tiLabel
         c <- liftIO $ panel nb []
         page <- go c =<< getDynamic tiBody
         liftIO $ do
           set c [ layout := page ]
           notebookAddPage nb c lab True (-1)
       liftIO $ notebookSetSelection nb 0
       pure (fill $ margin 5 $ widget nb)

     PlainText tv -> liftIO $ do
       txt <- getDynamic tv
       p' <- panel p []
       lo <- margin 5 . floatCentre . widget <$> staticText p' [ text := txt ]
       set p' [layout := lo]
       pure (hstretch . expand . margin 5 $ widget p')

     Button tv -> liftIO $ do
       txt <- getDynamic tv
       p' <- panel p []
       btn <- button p' [ text := txt
                        , on command := buttonPress txt ]
       pure (hstretch . expand . container p' $ floatCentre $ margin 5 $ widget btn)

     ColorPicker l _ col -> do
       Label lab <- getDynamic l
       (r0, g0, b0) <- colorToRGB . fromRight (const grey) <$> getDynamic col
       picker <- liftIO $ do
         picker <- feed2 [ text := lab, visible := True ] 0 $
           initialWindow $ \iD rect' ps s -> do
             e <- colorPickerCtrlCreate p iD (rgb r0 g0 b0) rect' s
             set e ps
             pure e
         let newPick = do
               c <- colorPickerCtrlGetColour picker
               let r = fromIntegral (colorRed c   :: Word8) / 255.0 :: Double
                   g = fromIntegral (colorGreen c :: Word8) / 255.0 :: Double
                   b = fromIntegral (colorBlue c  :: Word8) / 255.0 :: Double
               setValue (source col) (printf "rgb(%0.3f, %0.3f, %0.3f)" r g b :: String)

         -- TODO: update picker if color is changed by a script

         WX.windowOnEvent picker [wxEVT_COMMAND_COLOURPICKER_CHANGED] newPick (const newPick)
         pure picker

       pure (hstretch $ expand $ margin 5 $
             row 5 [ margin 3 (label lab), hfill (widget picker) ])

     CheckBox l _ b -> do
       Label lab <- getDynamic l
       initial <- getDynamic b
       cb <- liftIO $ do
         cb <- checkBox p [ text := lab
                          , checkable := True
                          , checked := initial
                          , visible := True
                          ]
         set cb [ on command := do
                    isChecked <- get cb checked
                    void (setValue b isChecked)
                ]
         pure cb
       watch b (\isChecked -> set cb [ checked := isChecked ])
       pure (hstretch . expand . margin 5 $ widget cb)

     ScriptBox v -> liftIO $ do
       txt <- unCodeString <$> getDynamic (source $ scriptCode v)
       p' <- panel p []
       ce <- codeEditor p' txt
       lastText <- newIORef txt
       set ce [ on focus := \tf -> do
                  case tf of
                    True -> styledTextCtrlGetText ce >>= writeIORef lastText
                    False -> do
                      new <- styledTextCtrlGetText ce
                      old <- readIORef lastText
                      when (new /= old) $ do
                        writeIORef lastText new
                        void (setValue (source $ scriptCode v) (CodeString new))
                  propagateEvent
              ]
       pure (container p' $ fill $ widget ce)

     TextBox l v -> do
       Label lab <- getDynamic l
       te <- liftIO $ do
         initial <- getDynamic (source $ exprValue v)
         te <- textEntry p [ text := initial
                           , processEnter := True
                           , tooltip := ""
                           ]
         normalBG <- get te bgcolor

         errorMessage <- variable [value := Nothing]

         errorPopup <- frame
           [ visible := False
           , style := wxFRAME_TOOL_WINDOW .+. wxNO_BORDER
           , position := Point 0 0
           ]
         ep <- panel errorPopup [ bgcolor := rgb 255 200 (200 :: Int)]
         txt <- staticText ep [ text := ""
                              , font := fontFixed
                              , fontSize := 12
                              , color := black ]

         skipNextFocusLoss <- variable [ value := False ]
         skipNextFocusGain <- variable [ value := False ]

         let updateAlertStatus = do
               get errorMessage value >>= \case
                 Nothing -> do
                   set te [ bgcolor := normalBG ]
                   set errorPopup [ visible := False ]
                 Just err -> do
                   set te [ bgcolor := rgb 180 80 (80 :: Int)]
                   Point wx wy <- windowGetScreenPosition te
                   set txt [ text := err ]
                   set skipNextFocusLoss [ value := True ]
                   set skipNextFocusGain [ value := True ]
                   set errorPopup [ layout := fill $ container ep $ margin 15 $ widget txt
                                  , position := Point (wx + 20) (wy + 60)
                                  , visible := True ]
                   set frame0 [ visible := True ]

         {-
         let setErrorMessage msg = do
               oldMsg <- get errorMessage value
               when (oldMsg /= msg) $ do
                 set errorMessage [value := msg]
                 updateAlertStatus
-}

         set te [ on command := do
                    newText <- get te text
                    setValue (source $ exprValue v) newText -- >>= setErrorMessage
                , on focus := \case
                    True -> do
                      get skipNextFocusGain value >>= \case
                        True  -> set skipNextFocusGain [ value := False ]
                        False -> updateAlertStatus
                      propagateEvent
                    False -> do
                      get skipNextFocusLoss value >>= \case
                        True  ->  set skipNextFocusLoss [ value := False ]
                        False -> do
                          newText <- get te text
                          oldText <- getDynamic (source $ exprValue v)
                          when (newText /= oldText) $ do
                            setValue (source $ exprValue v) newText -- >>= \msg -> do
                            --  setErrorMessage msg
                            --  set errorPopup [ visible := False ]

                      propagateEvent
                ]
         pure te
       watch (source $ exprValue v) (\newText -> set te [ text := newText ])
       pure (hstretch $ expand $ margin 5 $ row 5 [ margin 3 (label lab), hfill (widget te) ])
