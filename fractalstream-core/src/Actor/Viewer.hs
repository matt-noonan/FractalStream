module Actor.Viewer
  ( Viewer(..)
  ) where

import Language.Code
import Language.Effect.Render
import Language.Effect.Draw
import Actor
import Actor.Settings
import Actor.Tool
import Event

data Viewer where
  Viewer :: forall env
          . ( NotPresent "viewWidth" env
            , NotPresent "viewHeight" env )
         =>
    { viewerSettings :: Settings env NoEffects
    , onResize ::
        Code '[Render]
             ( '("viewWidth",  'IntegerT) ': '("viewHeight", 'IntegerT) ': env)
             'VoidT
    , onTimer :: Maybe (Code '[Render] env 'VoidT)
    , viewerTools :: [Tool '[Draw]]
    } -> Viewer

instance Actor Viewer where
  handle evt Viewer{..} = case evt of
    Timer -> SomeCode <$> onTimer
    Resize (_w,_h) -> Just $ SomeCode onResize
    _ -> Nothing
