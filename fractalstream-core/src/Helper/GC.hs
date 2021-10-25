module Helper.GC
  ( GarbageCollector(..)
  , registerRoot
  ) where

data GarbageCollector a = GarbageCollector
  { gcScan     :: IO [a]
  , gcInterval :: _
  , retain     :: a -> IO ()
  , release    :: a -> IO ()
  }

runGC :: GarbageCollector a -> IO ()
runGC GarbageCollector{..} = do
