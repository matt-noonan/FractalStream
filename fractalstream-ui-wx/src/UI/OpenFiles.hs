{- |
Module       : UI.OpenFiles
Description  : Open the projects the operating system asks FractalStream to open
-}
module UI.OpenFiles
  ( onOpenFiles
  ) where

import Data.List (isPrefixOf)
import Foreign.C.String (CString)
import Foreign.Ptr (FunPtr, castFunPtrToPtr, Ptr)
import qualified GHC.Foreign as GHC
import System.Environment (getArgs)
import System.IO (utf8)

-- | Call the action on each file the operating system asks FractalStream to
-- open. On Windows and Linux, opening a project from the file manager runs
-- @FractalStream <file>@, so these come from the command line. On macOS, they
-- arrive as events instead (at launch, or while FractalStream is running),
-- which wxc passes on through its open-file handler.
onOpenFiles :: (FilePath -> IO ()) -> IO ()
onOpenFiles open = do
  -- Skip options, such as the -psn_... argument that older versions of
  -- macOS pass to programs launched from the Finder.
  mapM_ open . filter (not . ("-" `isPrefixOf`)) =<< getArgs
  -- The handler is never freed: wxc keeps it for as long as the app runs.
  handler <- wrapOpenFileHandler (\path -> GHC.peekCString utf8 path >>= open)
  setOpenFileHandler (castFunPtrToPtr handler)

foreign import ccall "wrapper"
  wrapOpenFileHandler :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

foreign import ccall "ELJApp_SetOpenFileHandler"
  setOpenFileHandler :: Ptr () -> IO ()
