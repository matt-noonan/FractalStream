module Task.ConcurrentSpec (spec) where

import Test.Hspec

import Task.Concurrent

import Control.Concurrent
import Control.Monad (forever, replicateM, replicateM_)
import System.Timeout (timeout)

spec :: Spec
spec = describe "a synchronized resource" $
  it "stays usable however often its users are cancelled" $ do
    -- Users (like a tile's render workers, cancelled on every redraw)
    -- killed at random moments, over and over; then exclusive access (like
    -- painting the tile) has to still be possible. A user that was killed
    -- without giving up its place would block it for good.
    res <- synchronized ()
    let use = with res (\_ -> threadDelay 10)
    replicateM_ 300 $ do
      users <- replicateM 8 (forkIO (forever use))
      threadDelay 200
      mapM_ killThread users
      -- (and a cancelled exclusive access, too)
      painter <- forkIO (forever (synchedWith res (\_ -> pure ())))
      threadDelay 50
      killThread painter
    done <- timeout 5000000 (synchedWith res (\_ -> pure ()))
    done `shouldBe` Just ()
