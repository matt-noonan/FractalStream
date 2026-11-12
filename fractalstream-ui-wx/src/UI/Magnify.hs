{- |
Module      : UI.Magnify
Description : Trackpad pinches, which wxHaskell doesn't bind

wxWidgets reports a trackpad pinch as @wxEVT_MAGNIFY@ mouse events, each
with its magnification; wxHaskell binds neither, so a little C++
(@cbits/magnify.cpp@) connects them. (On Windows, where wxWidgets is a
prebuilt DLL, there are none: @cbits/magnify-none.c@.)
-}
module UI.Magnify
  ( onPinch
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr)
import Foreign.C.Types (CDouble(..), CInt(..))
import Graphics.UI.WXCore.WxcObject (withObjectPtr)
import Graphics.UI.WXCore.WxcClassTypes (Window)

type Callback = CDouble -> CInt -> CInt -> IO ()

foreign import ccall "wrapper"
  wrapCallback :: Callback -> IO (FunPtr Callback)

foreign import ccall unsafe "fs_on_magnify"
  fsOnMagnify :: Ptr () -> FunPtr Callback -> IO ()

-- | Do something with each pinch event on a window: its magnification
-- (the change in scale since the last event: positive to zoom in), and
-- the pointer's position in the window. (wxWidgets 3.2 doesn't say where
-- in the gesture an event is: a pinch is over when they stop.) The
-- callback lives as long as the program: a viewer's window binds one.
onPinch :: Window a -> (Double -> (Int, Int) -> IO ()) -> IO ()
onPinch w act = do
  cb <- wrapCallback $ \m x y -> act (realToFrac m) (fromIntegral x, fromIntegral y)
  withObjectPtr w $ \p -> fsOnMagnify (castPtr p) cb
