{-# LANGUAGE ForeignFunctionInterface #-}
-- | Following the system's light or dark appearance while a window is
-- open.
--
-- wxWidgets 3.2 on macOS works out a window's colours from the system's
-- when it's made, and doesn't look at them again: after the appearance
-- changes, native controls redraw themselves in the new one, but the
-- windows around them keep the old background, so that (say) a label's
-- new light text is drawn on the old light background. A dialog made
-- afresh each time is fine; a window that stays open, like the library
-- or the configuration window, isn't. So when the system's colours
-- change, 'followAppearance' gives the window and everything in it their
-- default colours again (the null colour: "whatever the system's are"),
-- and then runs the window's own action, for the colours it chose itself.
module UI.Appearance
  ( followAppearance
  , systemColour
  ) where

import Graphics.UI.WXCore

import Control.Monad (void)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)

-- (wxc's own functions: wxHaskell's setters always make a colour from
-- red, green and blue, and there's no such colour for "the default")
foreign import ccall unsafe "Null_Colour"
  wxcNullColour :: IO (Ptr ())
foreign import ccall unsafe "wxWindow_SetBackgroundColour"
  wxcSetBackgroundColour :: Ptr () -> Ptr () -> IO CInt

-- | When the system's appearance changes, give the window and everything
-- in it the system's colours again, then run the action (for colours the
-- window sets itself), and redraw.
followAppearance :: Window a -> IO () -> IO ()
followAppearance w reapply =
  windowOnEvent w [wxEVT_SYS_COLOUR_CHANGED] () $ \_ -> do
    resetColours (objectCast w)
    reapply
    windowRefresh w True
    propagateEvent

-- (Only the backgrounds of the windows wxWidgets draws itself, panels
-- and top-level windows: native controls, and text, follow the
-- appearance by themselves. A top-level window asserts on the null
-- colour, so it's given the system's window background as it is now.)
resetColours :: Window () -> IO ()
resetColours w = do
  if instanceOf w classTopLevelWindow
    then systemColour ColorBtnFace >>= void . windowSetBackgroundColour w
    else when (instanceOf w classPanel) $ do
      null' <- wxcNullColour
      withObjectPtr w $ \p -> void (wxcSetBackgroundColour (castPtr p) null')
  windowChildren w >>= mapM_ resetColours

-- | One of the system's colours, as it is now. (wxHaskell's
-- 'colorSystem' is pure, so a program can end up asking once and keeping
-- the answer, which goes stale when the appearance changes.)
systemColour :: SystemColor -> IO Color
systemColour c = systemSettingsGetColour (fromEnum c)
