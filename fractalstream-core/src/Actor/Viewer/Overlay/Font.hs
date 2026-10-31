{-# language TemplateHaskell #-}
{- |
Module       : Actor.Viewer.Overlay.Font
Description  : The font 'Actor.Viewer.Overlay' draws text with

A copy of DejaVu Sans (see @fonts/DejaVuSans-LICENSE@), embedded in the
library so that rasterizing text doesn't depend on locating a font file at
run time. This lives in its own module to keep the Template Haskell splice
away from everything else.
-}
module Actor.Viewer.Overlay.Font
  ( overlayFont
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.FileEmbed (embedFile)
import Graphics.Text.TrueType (Font, decodeFont)

dejaVuSansFile :: ByteString
dejaVuSansFile = $(embedFile "fonts/DejaVuSans.ttf")

-- | The font text is drawn in, or 'Nothing' (so text is skipped) if the
-- embedded font somehow fails to decode.
overlayFont :: Maybe Font
overlayFont = either (const Nothing) Just (decodeFont (LB.fromStrict dejaVuSansFile))
{-# NOINLINE overlayFont #-}
