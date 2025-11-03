module UI.Layout
  ( generateWxLayout
  ) where

import FractalStream.Prelude hiding (get)

import Data.Color (colorToRGB, rgbToColor)
import Data.DynamicValue
import Actor.Layout
import Data.IORef

import UI.CodeEditor

import Graphics.UI.WX hiding (pt, glue, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color)
import qualified Graphics.UI.WXCore.Events as WX
import qualified Graphics.UI.WX as WX
import           Graphics.UI.WXCore.WxcClassesAL
import           Graphics.UI.WXCore.WxcClassesMZ
import           Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.Frame (windowGetScreenPosition)

generateWxLayout :: Dynamic dyn
                 => (String -> IO ())
                 -> Window a
                 -> Layout dyn
                 -> IO WX.Layout

generateWxLayout buttonPress frame0 wLayout = do
  panel0 <- panel frame0 []
  computedLayout <- go panel0 wLayout
  pure (container panel0 $ fill $ computedLayout)

 where

   go :: Dynamic dyn => Window a -> Layout dyn -> IO WX.Layout
   go p = \case

     Panel pTitle inner -> do
       p' <- panel p [ ]
       lo <- go p' inner
       set p' [ layout := lo ]
       pure (hstretch $ expand $ margin 5 $ boxed pTitle (fill $ widget p'))

     Vertical parts -> do
       p' <- panel p []
       lo <- hstretch . expand . margin 5 . column 5 <$> mapM (go p') parts
       set p' [ layout := lo ]
       pure (hstretch $ expand (widget p'))

     Horizontal parts -> do
       p' <- panel p []
       lo <- fill . margin 5 . row 5 <$> mapM (go p') parts
       set p' [ layout := lo ]
       pure (hstretch $ expand (widget p'))

     Tabbed ts -> do
       nb <- feed2 [ visible := True ] 0 $
             initialWindow $ \iD rect' ps s -> do
                   e <- notebookCreate p iD rect' s
                   set e ps
                   pure e
       forM_ ts $ \(lab, x) -> do
         c <- panel nb []
         page <- go c x
         set c [ layout := page ]
         notebookAddPage nb c lab True (-1)
       notebookSetSelection nb 0
       pure (fill $ margin 5 $ widget nb)

     PlainText txt -> do
       p' <- panel p []
       lo <- margin 5 . floatCentre . widget <$> staticText p' [ text := txt ]
       set p' [layout := lo]
       pure (hstretch . expand . margin 5 $ widget p')

     Button txt -> do
       p' <- panel p []
       btn <- button p' [ text := txt
                        , on command := buttonPress txt ]
       pure (hstretch . expand . container p' $ floatCentre $ margin 5 $ widget btn)

     ColorPicker (Label lab) v -> do
       (r0, g0, b0) <- colorToRGB <$> getDynamic v
       picker <- feed2 [ text := lab, visible := True ] 0 $
                 initialWindow $ \iD rect' ps s -> do
                   e <- colorPickerCtrlCreate p iD (rgb r0 g0 b0) rect' s
                   set e ps
                   pure e
       let newPick = do
             c <- colorPickerCtrlGetColour picker
             let r = colorRed c
                 g = colorGreen c
                 b = colorBlue c
             void (setDynamic v (rgbToColor (r, g, b)))
       WX.windowOnEvent picker [wxEVT_COMMAND_COLOURPICKER_CHANGED] newPick (const newPick)
       pure (hstretch $ expand $ margin 5 $
             row 5 [ margin 3 (label lab), hfill (widget picker) ])

     CheckBox (Label lab) v -> do
       initial <- getDynamic v
       cb <- checkBox p [ text := lab
                        , checkable := True
                        , checked := initial
                        , visible := True
                        ]
       set cb [ on command := do
                  isChecked <- get cb checked
                  void (setDynamic v isChecked)
              ]
       listenWith v (\_ isChecked -> set cb [ checked := isChecked ])
       pure (hstretch . expand . margin 5 $ widget cb)

     Multiline v -> do
       txt <- unCodeString <$> getDynamic v
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
                        void (setDynamic v (CodeString new))
                  propagateEvent
              ]
       pure (container p' $ fill $ widget ce)

     TextBox (Label lab) v -> do
       initial <- getDynamic v
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

       let setErrorMessage msg = do
             oldMsg <- get errorMessage value
             when (oldMsg /= msg) $ do
               set errorMessage [value := msg]
               updateAlertStatus

       set te [ on command := do
                  newText <- get te text
                  setDynamic v newText >>= setErrorMessage
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
                        oldText <- getDynamic v
                        when (newText /= oldText) $ do
                          setDynamic v newText >>= \msg -> do
                            setErrorMessage msg
                            set errorPopup [ visible := False ]

                    propagateEvent
              ]
       listenWith v (\_ newText -> set te [ text := newText ])
       pure (hstretch $ expand $ margin 5 $ row 5 [ margin 3 (label lab), hfill (widget te) ])
