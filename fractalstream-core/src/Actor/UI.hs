module Actor.UI
  ( UI(..)
  ) where

import Data.DynamicValue
import Actor.Layout
import Actor.Viewer.Complex

data UI where
  UI :: forall ensembleHandle.
      { newEnsemble :: IO ensembleHandle
      , runSetup :: ensembleHandle
                 -> String
                 -> Layout SomeDynamic
                 -> IO ()
                 -> IO ()
      , makeLayout :: ensembleHandle
                   -> String
                   -> Layout SomeDynamic
                   -> IO ()
      , makeViewer :: ensembleHandle
                   -> ViewerUIProperties
                   -> ComplexViewer'
                   -> IO ()
      } -> UI
