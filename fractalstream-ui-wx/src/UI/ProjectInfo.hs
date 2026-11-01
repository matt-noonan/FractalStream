-- | The "Project info" dialog: view and edit a project's
-- @metadata:@ title and description, and see its current preview image.
-- (Its preview commands are left alone: they're edited along with the
-- rest of the project file, and run by @fsrun --update-preview@.)
module UI.ProjectInfo
  ( editProjectInfo
  ) where

import FractalStream.Prelude hiding (get)

import Actor.Ensemble (Ensemble(..))
import Actor.Metadata (ProjectMetadata(..))
import Data.DynamicValue

import Graphics.UI.WX hiding (pt, when, Color, Object, Layout, Vertical, Horizontal)
import qualified Graphics.UI.WX as WX

import UI.PreviewImage (previewImage)

import Data.Char (isSpace)

-- | Show the dialog for whichever 'Ensemble' the session is currently
-- running. If the user changes anything and clicks OK, the metadata is
-- updated and @markUnsaved@ is run.
editProjectInfo :: Window a -> Variable Ensemble -> IO () -> IO ()
editProjectInfo owner currentEnsemble markUnsaved = do
  prj <- getDynamic currentEnsemble
  md <- getDynamic (ensembleMetadata prj)

  d <- dialog owner [ text := "Project info", resizeable := True ]
  p <- panel d []

  titleEntry <- textEntry p [ text := fromMaybe "" (mdTitle md) ]
  descriptionEntry <- textCtrl p [ text := fromMaybe "" (mdDescription md) ]

  previewWidget <- case mdPreview md >>= previewImage of
    Nothing -> fmap widget . staticText p $ [ text :=
      "No preview yet. To make one, choose \"Use this as preview\" from a viewer's menu." ]
    Just (w, h, makeImage) -> do
      img <- makeImage
      imagePanel <- panel p [ on paint := \dc _ -> drawImage dc img (WX.pt 0 0) [] ]
      pure (minsize (sz w h) (widget imagePanel))

  ok <- button p [ text := "OK" ]
  cancel <- button p [ text := "Cancel" ]

  -- When the dialog is resized, the description takes up all the new
  -- room, the title only grows sideways, and the preview stays as is,
  -- centered. A 'column' only gets wider if /every/ item in it can
  -- stretch sideways, so even the items that shouldn't grow (the
  -- description's label, the preview) are 'hstretch'ed -- which just
  -- gives them a wider space to sit in, aligned as each one says.
  set d [ layout := container p $ margin 10 $ column 5
            [ row 5 [ label "Title:", hfill (widget titleEntry) ]
            , hstretch (label "Description:")
            , fill (minsize (sz 420 120) (widget descriptionEntry))
            , hfloatCentre previewWidget
            , row 5 [ widget cancel, hglue, widget ok ]
            ]
        ]


  result <- showModal d $ \done -> do
    set cancel [ on command := done Nothing ]
    set ok [ on command := do
               newTitle <- get titleEntry text
               newDescription <- get descriptionEntry text
               done (Just md { mdTitle = nonEmpty newTitle
                             , mdDescription = nonEmpty newDescription }) ]


  forM_ result $ \md' -> when (md' /= md) $ do
    setValue' (ensembleMetadata prj) md'
    markUnsaved
  where
    nonEmpty s = if all isSpace s then Nothing else Just s
