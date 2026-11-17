{- |
Module      : UI.BundledFont
Description : The editors' default font, from FractalStream itself

The editors' default font is DejaVu Sans ('FractalStream.Settings.defaultEditorFont'),
which covers the symbols scripts use (θ, ℂ, ∘, ...) where the platforms'
usual fixed-width fonts often don't. It's rarely installed on Windows or
macOS, so FractalStream adds the copy it carries
("Actor.Viewer.Overlay.Font") to its own process's fonts, by name, before
any editor asks for it (@cbits/bundled-font.c@). On Linux, it uses the
system's DejaVu Sans, which is usually there.
-}
module UI.BundledFont
  ( addBundledFont
  ) where

import Actor.Viewer.Overlay.Font (dejaVuSansFile)
import qualified Data.ByteString.Unsafe as BU
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr (Ptr, castPtr)

foreign import ccall unsafe "fs_add_bundled_font"
  fsAddBundledFont :: Ptr () -> CSize -> IO CInt

-- | Add DejaVu Sans to the process's fonts: whether that worked.
addBundledFont :: IO Bool
addBundledFont = BU.unsafeUseAsCStringLen dejaVuSansFile $ \(p, n) ->
  (/= 0) <$> fsAddBundledFont (castPtr p) (fromIntegral n)
