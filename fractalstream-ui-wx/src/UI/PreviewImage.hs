-- | Turning a project's preview (a PNG image, see "Actor.Metadata")
-- into a wx image to show. A preview may be partly transparent (a
-- drawn icon rather than a render, say), so it's always shown over a
-- background color.
module UI.PreviewImage
  ( previewImage
  , paddedPreviewImage
  ) where

import Codec.Picture (decodePng, convertRGBA8, Image(..), PixelRGBA8(..), pixelAt)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Graphics.UI.WX (Color, rgb, colorRed, colorGreen, colorBlue, sz, imageCreateFromPixels)
import qualified Graphics.UI.WXCore.WxcClassTypes as WXC

-- | A preview over the given background color, as a wx image along
-- with its width and height, if its PNG can be decoded.
previewImage :: Color -> ByteString -> Maybe (Int, Int, IO (WXC.Image ()))
previewImage background png = decodeRGBA8 png >>= \img ->
  let (w, h) = (imageWidth img, imageHeight img)
  in Just (w, h, imageCreateFromPixels (sz w h)
                   [ over background (pixelAt img x y) | y <- [0 .. h - 1], x <- [0 .. w - 1] ])

-- | A preview centered on a @side@ by @side@ square of the given
-- background color (or just that background, without a preview), for
-- showing previews of different shapes side by side.
paddedPreviewImage :: Int -> Color -> Maybe ByteString -> IO (WXC.Image ())
paddedPreviewImage side background mpng =
  imageCreateFromPixels (sz side side)
    [ pixel x y | y <- [0 .. side - 1], x <- [0 .. side - 1] ]
  where
    mimg = mpng >>= decodeRGBA8
    pixel x y = case mimg of
      Just img
        | let x' = x - (side - imageWidth img) `div` 2
              y' = y - (side - imageHeight img) `div` 2
        , x' >= 0, y' >= 0, x' < imageWidth img, y' < imageHeight img
        -> over background (pixelAt img x' y')
      _ -> background

decodeRGBA8 :: ByteString -> Maybe (Image PixelRGBA8)
decodeRGBA8 = either (const Nothing) (Just . convertRGBA8) . decodePng

-- | A pixel drawn over a background color, by its opacity.
over :: Color -> PixelRGBA8 -> Color
over background (PixelRGBA8 r g b a) =
  rgb (mix r (colorRed background)) (mix g (colorGreen background)) (mix b (colorBlue background))
  where
    mix :: Word8 -> Int -> Int
    mix c bg = (fromIntegral c * fromIntegral a + bg * (255 - fromIntegral a)) `div` 255
