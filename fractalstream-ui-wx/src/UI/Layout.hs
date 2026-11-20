module UI.Layout
  ( generateWxLayout
  , LayoutHooks(..)
  , noAppWidgets
  ) where

import FractalStream.Prelude hiding (get)

import Data.DynamicValue
import Actor.Layout

import Graphics.UI.WX hiding (pt, grey, glue, when, tool, Object, Dimensions, Horizontal, Vertical, Layout, Color)
import qualified Graphics.UI.WX as WX

import UI.Widgets

import Actor.TableTransfer
import Data.Zip (Packing(..))
import Control.Exception (SomeException, displayException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Language.Type (SomeType(..))
import System.FilePath (takeExtension)

-- | What a layout's widgets do in the app.
data LayoutHooks = LayoutHooks
  { lhButtonPress :: String -> IO ()
    -- ^ A button's press, with its text (for a tool's handlers).
  , lhAppWidget :: forall b. Window b -> AppWidget -> IO WX.Layout
    -- ^ The widgets of an 'AppWidget', in this window.
  , lhPages :: Maybe PageMemory
    -- ^ Where its menus of pages remember the page they show.
  }

-- | For a layout with no 'AppWidget's (a tool's).
noAppWidgets :: Window b -> AppWidget -> IO WX.Layout
noAppWidgets _ _ = pure WX.empty

-- | Build a layout's widgets.
generateWxLayout :: LayoutHooks
                 -> Window a
                 -> Layout
                 -> IO (IO (), WX.Layout)

generateWxLayout LayoutHooks{..} frame0 wLayout = do
  panel0 <- panel frame0 []

  -- (`pagePath`: the labels of the pages this is on, for remembering pages)
  let go :: [String] -> Window a -> Layout -> StateT (IO ()) IO WX.Layout
      go pagePath p = \case

        Panel pTitle innerLayout -> do
          Label title <- getDynamic pTitle
          p' <- liftIO $ panel p [ ]
          inner <- getDynamic innerLayout
          lo <- go pagePath p' inner
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
            _  -> hstretch . expand . margin 5 . column 5 <$> mapM (go pagePath p') parts
          liftIO $ set p' [ layout := lo ]
          pure (hstretch $ expand (widget p'))

        Horizontal v -> do
          p' <- liftIO $ panel p []
          parts <- getDynamic v
          lo <- case parts of
            [] -> pure WX.empty
            _  -> fill . margin 5 . row 5 <$> mapM (go pagePath p') parts
          liftIO $ set p' [ layout := lo ]
          pure (hstretch $ expand (widget p'))

        Tabbed dts -> do
          -- (a menu of pages, not a notebook's crowded tabs)
          book <- liftIO $ choiceBook ((, pagePath) <$> lhPages) p
          ts <- getDynamic dts
          pages <- forM ts $ \TabItem{..} -> do
            Label lab <- getDynamic tiLabel
            c <- liftIO $ choiceBookPage book
            page <- go (pagePath ++ [lab]) c =<< getDynamic tiBody
            pure (lab, c, page)
          lo <- liftIO $ choiceBookLayout book pages
          pure (fill $ margin 5 lo)

        PlainText tv -> liftIO $ do
          p' <- panel p []
          txt <- plainTextWidget p' tv
          set p' [layout := margin 5 . floatCentre . widget $ txt ]
          pure (hstretch . expand . margin 5 $ widget p')

        Button tv -> liftIO $ do
          p' <- panel p []
          btn <- buttonWidget p' tv
          set btn [ on command := get btn text >>= lhButtonPress ]
          pure (hstretch . expand . container p' $ floatCentre $ margin 5 $ widget btn)

        -- (a table: what it is, its size as it changes, and Export… and
        -- Import…: see Actor.TableTransfer)
        TableField name st@(SomeType ty) var -> liftIO $ do
          p' <- panel p []
          nameTxt <- staticText p' [ text := name ]
          summary <- staticText p' []
          let showSummary t = set summary [ text := tableSummary st t ] >> windowReLayout p'
          getDynamic var >>= showSummary
          wxWatchDynamic p' var showSummary
          let zipType = ("FractalStream table (.zip)", ["*.zip"])
              csvType = ("CSV (.csv)", ["*.csv"])
              csv = csvAvailable ty
          exportBtn <- button p' [ text := "Export…" ]
          importBtn <- button p' [ text := "Import…" ]
          set exportBtn [ on command := do
            picked <- fileSaveDialog p' True True ("Export the table " ++ name)
                        (zipType : [ csvType | csv ]) "" (name ++ "-table.zip")
            forM_ picked $ \path -> do
              let format = if map toLower (takeExtension path) == ".csv" && csv then CsvFormat else ZipFormat
              exportTableFile Deflated format name st var >>= \case
                Left why -> errorDialog p' "Couldn't export the table" why
                Right bytes -> try (BL.writeFile path bytes) >>= \case
                  Right () -> pure ()
                  Left (e :: SomeException) -> errorDialog p' "Couldn't export the table" (displayException e) ]
          set importBtn [ on command := do
            picked <- fileOpenDialog p' True True ("Import into the table " ++ name)
                        (if csv then [("FractalStream table or CSV", ["*.zip", "*.csv"]), zipType, csvType]
                                else [zipType]) "" ""
            forM_ picked $ \path -> try (BS.readFile path) >>= \case
              Left (e :: SomeException) -> errorDialog p' "Couldn't import the table" (displayException e)
              Right bytes -> importTableFile st var bytes >>= \case
                Left why -> errorDialog p' "Couldn't import the table" why
                Right () -> pure () ]
          set p' [ layout := row 5 [ margin 3 (widget nameTxt), margin 3 (hfill (widget summary))
                                   , widget exportBtn, widget importBtn ] ]
          pure (hstretch . expand . margin 5 $ widget p')

        AppWidgetL w -> liftIO $ do
          p' <- panel p []
          lo <- lhAppWidget p' w
          set p' [ layout := lo ]
          pure (hstretch . expand . margin 5 $ widget p')

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

        TextBox l v -> do
          (labelTxt, te, errorMessage) <- expressionWidget p l v
          pure (hstretch $ expand $ margin 5 $ row 5 [ margin 3 (widget labelTxt)
                                                     , hfill (widget te)
                                                     , widget errorMessage ])

  (computedLayout, done) <- runStateT (go [] panel0 wLayout) (pure ())
  pure (done, container panel0 $ fill  computedLayout)
