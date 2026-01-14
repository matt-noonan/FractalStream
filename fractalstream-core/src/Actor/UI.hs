module Actor.UI
  ( UI(..)
  ) where


import Actor.Layout
import Actor.Viewer.Complex

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
--                   -> ViewerUIProperties
                   -> ComplexViewer
                   -> IO ()
      } -> UI
