-- | Turning a project's preview (a PNG image, see "Actor.Metadata")
-- into a wx image to show.
module UI.PreviewImage
  ( previewImage
  , paddedPreviewImage
  ) where

import Codec.Picture (decodePng, convertRGB8, Image(..), PixelRGB8(..), pixelAt)
import Data.ByteString (ByteString)
import Graphics.UI.WX (Color, rgb, sz, imageCreateFromPixels)
import qualified Graphics.UI.WXCore.WxcClassTypes as WXC

-- | A preview, as a wx image along with its width and height, if its
-- PNG can be decoded.
previewImage :: ByteString -> Maybe (Int, Int, IO (WXC.Image ()))
previewImage png = decodeRGB8 png >>= \img ->
  let (w, h) = (imageWidth img, imageHeight img)
  in Just (w, h, imageCreateFromPixels (sz w h)
                   [ toColor (pixelAt img x y) | y <- [0 .. h - 1], x <- [0 .. w - 1] ])

-- | A preview centered on a @side@ by @side@ square of the given
-- background color (or just that background, without a preview), for
-- showing previews of different shapes side by side.
paddedPreviewImage :: Int -> Color -> Maybe ByteString -> IO (WXC.Image ())
paddedPreviewImage side background mpng =
  imageCreateFromPixels (sz side side)
    [ pixel x y | y <- [0 .. side - 1], x <- [0 .. side - 1] ]
  where
    mimg = mpng >>= decodeRGB8
    pixel x y = case mimg of
      Just img
        | let x' = x - (side - imageWidth img) `div` 2
              y' = y - (side - imageHeight img) `div` 2
        , x' >= 0, y' >= 0, x' < imageWidth img, y' < imageHeight img
        -> toColor (pixelAt img x' y')
      _ -> background

decodeRGB8 :: ByteString -> Maybe (Image PixelRGB8)
decodeRGB8 = either (const Nothing) (Just . convertRGB8) . decodePng

toColor :: PixelRGB8 -> Color
toColor (PixelRGB8 r g b) = rgb r g b
