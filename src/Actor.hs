module Actor
  ( Actor(..)
  , SomeActor(..)
  ) where

import Event
import Language.Code

data SomeActor where
  SomeActor :: forall actor. Actor actor => actor -> SomeActor

class Actor actor where

  startActor :: actor -> Maybe SomeCode
  startActor _ = Nothing

  stopActor  :: actor -> Maybe SomeCode
  stopActor _ = Nothing

  handle :: Event Scalar -> actor -> Maybe SomeCode
  handle _ _ = Nothing
