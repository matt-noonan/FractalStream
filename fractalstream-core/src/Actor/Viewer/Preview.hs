-- | Rendering a small preview image of a viewer, for a project's
-- @metadata:@ (see "Actor.Metadata"). Shared by every frontend, so that
-- a preview made in the app and one made by @fsrun --update-preview@
-- come out the same.
module Actor.Viewer.Preview
  ( renderPreview
  , previewLongSide
  ) where

import Actor.Layout (Dimensions(..))
import Actor.Viewer
import Actor.Viewer.Overlay (compositeDrawLayers)
import Data.DynamicValue

import Codec.Picture (encodePng)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL

-- | How many pixels the longer side of a preview image is.
previewLongSide :: Int
previewLongSide = 128

-- | Render a viewer's current view as a PNG preview image, scaled so
-- that its longer side is 'previewLongSide' pixels and its shape
-- matches the viewer's. It shows the same part of the model as the
-- viewer does -- the pixel size grows as the image shrinks -- rendered
-- afresh (and antialiased) at the smaller size, rather than scaled
-- down from the viewer's own image. Tools' drawings are included
-- unless @baseOnly@ is set, just like a scenario's @capture@ and
-- @capture ... base@.
renderPreview :: Bool -> Viewer -> IO (Either String ByteString)
renderPreview baseOnly viewer = do
  Dimensions (w, h) <- getDynamic (vSize viewer)
  let scale = fromIntegral previewLongSide / fromIntegral (max 1 (max w h)) :: Double
      scaled n = max 1 (round (fromIntegral n * scale))
  preview <- cloneViewer viewer
  setValue' (vSize preview) (Dimensions (scaled w, scaled h))
  modifyValue (vPixelSize preview) (/ scale)
  fmap (BSL.toStrict . encodePng . snd) <$>
    withRenderedViewerBuffer preview True (\center pixelSize dims buf -> do
      layers <- if baseOnly then pure [] else vDrawCmds viewer
      compositeDrawLayers center pixelSize layers dims buf)
