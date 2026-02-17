module UI.Layout
  ( generateWxLayout
  ) where

import FractalStream.Prelude hiding (get)

import Data.DynamicValue
import Language.Parser.SourceRange (spanOfSourceRange, SourceSpan(..))
import Language.Parser.Tokenizer (commentRanges)
import Actor.Layout
import Data.IORef
import qualified Data.Map as Map
import Control.Concurrent

import UI.CodeEditor

import Graphics.UI.WX hiding (pt, grey, glue, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color)
import qualified Graphics.UI.WX as WX
import           Graphics.UI.WXCore.WxcClassesMZ

import UI.Widgets

generateWxLayout :: (String -> IO ())
                 -> Window a
                 -> Layout
                 -> IO (IO (), WX.Layout)

generateWxLayout buttonPress frame0 wLayout = do
  panel0 <- panel frame0 []

  let watch :: forall f a. AsDynamic f => f a -> (a -> IO ()) -> StateT (IO ()) IO ()
      watch d action = do
        xv <- liftIO $ newMVar (pure ())
        t <- liftIO $ timer frame0 [ interval := 100, enabled := True
                                   , on command := do
                                       todo <- modifyMVar xv (pure . (pure (),))
                                       todo
                                   ]
        done <- watchDynamic d (\x -> modifyMVar_ xv (const $ pure $ action x))
        modify (timerStop t >> done >>)

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
          p' <- panel p []
          txt <- plainTextWidget p' tv
          set p' [layout := margin 5 . floatCentre . widget $ txt ]
          pure (hstretch . expand . margin 5 $ widget p')

        Button tv -> liftIO $ do
          p' <- panel p []
          btn <- buttonWidget p' tv
          set btn [ on command := get btn text >>= buttonPress ]
          pure (hstretch . expand . container p' $ floatCentre $ margin 5 $ widget btn)

        Selection _l v pick options -> do
          p' <- liftIO $ panel p []
          c <- selectionWidget p' v pick options
          pure (hstretch . expand . container p' $ fill $ widget c)

        ColorPicker l v col -> do
          (labelTxt, picker) <- colorWidget p l v col
          pure (hstretch $ expand $ margin 5 $
                row 5 [ margin 3 $ widget labelTxt, hfill (widget picker) ])

        CheckBox l v b -> do
          cb <- checkboxWidget p l v b
          pure (hstretch . expand . margin 5 $ widget cb)

        ScriptBox v -> do
          p' <- liftIO $ panel p []
          ce <- liftIO $ codeEditor p' (scriptCode v)
          txt <- unCodeString <$> getDynamic (source $ scriptCode v)
          lastText <- liftIO $ newIORef txt
          errorText <- liftIO $ staticText p' [ text := "" ]

          let doSyntaxColoring = do
                code <- get ce text
                if null code
                  then pure Map.empty
                  else do
                    let m = editorOffsetMap code
                    styledTextCtrlStartStyling ce 0 0
                    case Map.lookup (length code - 1) m of
                      Nothing -> pure ()
                      Just k -> styledTextCtrlSetStyling ce (k + 1) 0
                    forM_ (commentRanges code) $ \(s, e) -> do
                      case (,) <$> Map.lookup s m <*> Map.lookup (e + 1 - s) m of
                        Nothing -> pure ()
                        Just (styleStart, styleRange) -> do
                          styledTextCtrlStartStyling ce styleStart 0
                          styledTextCtrlSetStyling ce styleRange 2
                    pure m

          liftIO $ set ce $
            [ on focus := \tf -> do
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

          -- Check for script changes periodically. TODO: make this event-driven instead
          liftIO $ do
            t <- timer frame0 [ interval := 500, enabled := True
                              , on command := do
                                  new <- styledTextCtrlGetText ce
                                  old <- readIORef lastText
                                  when (new /= old) $ do
                                    writeIORef lastText new
                                    setValue (source $ scriptCode v) (CodeString new) ]
            set frame0 [ on closing :~ (timerStop t >>) ]
          watch (source $ scriptCode v) $ \(CodeString newText) -> do
            oldText <- styledTextCtrlGetText ce
            when (oldText /= newText) $
              styledTextCtrlSetText ce newText
          isError <- liftIO ((isLeft <$> getDynamic (scriptCode v)) >>= \startError ->
             variable [ value := startError ])
          watch (scriptCode v) $ \case
            Left (loc, msg) -> do
              set isError [ value := True ]
              set errorText [ text := "⚠️ " ++ msg, visible := True ]
              windowReLayout p'
              code <- styledTextCtrlGetText ce
              case convertSourceSpan code <$> spanOfSourceRange loc of
                Nothing -> void doSyntaxColoring
                Just (s, e) -> do
                  m <- doSyntaxColoring
                  case (,) <$> Map.lookup s m <*> Map.lookup (e + 1 - s) m of
                    Nothing -> pure ()
                    Just (styleStart, styleRange) -> do
                      styledTextCtrlStartStyling ce styleStart 0
                      styledTextCtrlSetStyling ce styleRange 1
            Right _ -> do
              wasError <- get isError value
              void doSyntaxColoring
              when wasError $ do
                set isError [ value := False ]
                set errorText [ text := "", visible := False ]
                windowReLayout p'

          void $ liftIO doSyntaxColoring
          pure (container p' $ fill $ column 5 [ fill $ widget ce, hstretch $ expand $ widget errorText ])

        TextBox l v -> do
          (labelTxt, te, errorMessage) <- expressionWidget p l v
          pure (hstretch $ expand $ margin 5 $ row 5 [ margin 3 (widget labelTxt)
                                                     , hfill (widget te)
                                                     , widget errorMessage ])

  (computedLayout, done) <- runStateT (go panel0 wLayout) (pure ())
  pure (done, container panel0 $ fill  computedLayout)

convertSourceSpan :: String -> SourceSpan -> (Int, Int)
convertSourceSpan input = \case
  InLine linum s e -> let lo = lineOffset input linum + s - 1
                          hi = e - s + lo
                      in (lo, hi)
  InRows s e -> let lo = lineOffset input
                in (lo s, lo (e + 1))

lineOffset :: String -> Int -> Int
lineOffset input = let m = Map.fromList
                         . ((0,0):)
                         . zip [1..]
                         . map fst
                         . filter ((== '\n') . snd)
                         . zip [1..] $ input
                       li = length input
                   in \lo -> 1 + Map.findWithDefault li lo m
