module Actor.UI
  ( UI(..)
  ) where


import Actor.Layout
--import Actor.Viewer.Complex

data TODO = TODO

data UI where
  UI :: forall ensembleHandle.
      { newEnsemble :: IO ensembleHandle
      , runSetup :: ensembleHandle
                 -> String
                 -> Layout
                 -> IO ()
                 -> IO ()
      , makeLayout :: ensembleHandle
                   -> String
                   -> Layout
                   -> IO ()
      , makeViewer :: ensembleHandle
                   -> TODO --ViewerUIProperties
                   -> TODO --ComplexViewer'
                   -> IO ()
      } -> UI
