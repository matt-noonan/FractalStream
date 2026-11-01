-- | Opening a folder in the system's file manager (Finder, Explorer, or
-- whatever handles folders on Linux).
module UI.ShowInFileManager
  ( showInFileManager
  ) where

import Control.Exception (IOException, try)
import Control.Monad (void)
import System.Info (os)
import System.Process (spawnProcess)

-- | Show a folder in the file manager. If that can't be done (no file
-- manager to be found, say), nothing happens: there's nothing better to
-- do about it.
showInFileManager :: FilePath -> IO ()
showInFileManager dir =
  void (try (void (spawnProcess opener [dir])) :: IO (Either IOException ()))
  where
    opener = case os of
      "darwin"  -> "open"
      "mingw32" -> "explorer"
      _         -> "xdg-open"
