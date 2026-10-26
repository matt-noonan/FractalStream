module UI.Layout
  ( generateWxLayout
  ) where

import FractalStream.Prelude hiding (get)

import Data.DynamicValue
import Actor.Layout

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

  let go :: Window a -> Layout -> StateT (IO ()) IO WX.Layout
      go p = \case

        Panel pTitle innerLayout -> do
          Label title <- getDynamic pTitle
          p' <- liftIO $ panel p [ ]
          inner <- getDynamic innerLayout
          lo <- go p' inner
          liftIO $ set p' [ layout := lo ]
          pure (hstretch $ expand $ margin 5 $ boxed title (fill $ widget p'))

        -- `column`/`row` route through wxHaskell's own `grid`, which
        -- calls `maximum` on its row lengths -- an empty list of
        -- sub-layouts (e.g. a `code <name>:` declaration's own
        -- invisible placeholder, or any stanza with no visible
        -- fields) would crash with `Prelude.maximum: empty list`, so
        -- skip straight to `WX.empty` (a zero-size layout) rather than
        -- ever calling `column`/`row 5` with nothing in it.
        Vertical v -> do
          p' <- liftIO $ panel p []
          parts <- getDynamic v
          lo <- case parts of
            [] -> pure WX.empty
            _  -> hstretch . expand . margin 5 . column 5 <$> mapM (go p') parts
          liftIO $ set p' [ layout := lo ]
          pure (hstretch $ expand (widget p'))

        Horizontal v -> do
          p' <- liftIO $ panel p []
          parts <- getDynamic v
          lo <- case parts of
            [] -> pure WX.empty
            _  -> fill . margin 5 . row 5 <$> mapM (go p') parts
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
          (ce, errorText) <- scriptWidget p' (scriptCode v)
          pure (container p' $ fill $ column 5 [ fill $ widget ce, hstretch $ expand $ widget errorText ])

        TextBox l v -> do
          (labelTxt, te, errorMessage) <- expressionWidget p l v
          pure (hstretch $ expand $ margin 5 $ row 5 [ margin 3 (widget labelTxt)
                                                     , hfill (widget te)
                                                     , widget errorMessage ])

  (computedLayout, done) <- runStateT (go panel0 wLayout) (pure ())
  pure (done, container panel0 $ fill  computedLayout)
