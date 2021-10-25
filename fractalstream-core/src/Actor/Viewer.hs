module Actor.Viewer
  ( Viewer(..)
  ) where

import Language.Code
import Language.Effect.Render
import Language.Effect.Draw
import Language.Effect.Provide
import Actor.Settings
import Actor.Tool
import Event

data Viewer where
  Viewer :: forall env
          . ( NotPresent "viewWidth" env
            , NotPresent "viewHeight" env )
         =>
    { viewerSettings :: Settings env NoEffects
    , viewToModel :: Value
                      '( '("viewX", 'RealT) ': '("viewY", 'RealT) ': env
                       , 'Pair 'RealT 'RealT)
    , modelToView :: Value
                      '( '("modelX", 'RealT) ': '("modelY", 'RealT) ': env
                       , 'Pair 'RealT 'RealT)
    , onRefresh :: Maybe (Code '[Provide env, Render] '[] 'VoidT)
    , onResize  :: Maybe (Code '[Provide env, Render] ResizeEnv 'VoidT)
    , onTimer   :: Maybe (Code '[Provide env, Render] '[] 'VoidT)
    , viewerTools :: [Tool '[Draw]]
    } -> Viewer

{-
instance Actor (Viewer env) '[Provide env, Render] where
  handle Viewer{..} _ = \case
    Timer   -> onTimer
    Resize  -> onResize
    Refresh -> onRefresh
    _       -> Nothing
-}
