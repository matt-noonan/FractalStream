module Event
  ( Event(..)
  ) where

import Language.Type

data Event (value :: Type -> *) where
  Refresh :: Event value
  Resize  :: (value 'IntegerT, value 'IntegerT) -> Event value
  Timer   :: Event value
  Click   :: (value 'IntegerT, value 'IntegerT) -> Event value
  MouseDown :: (value 'IntegerT, value 'IntegerT) -> Event value
  MouseUp   :: (value 'IntegerT, value 'IntegerT) -> Event value
  Motion    :: (value 'IntegerT, value 'IntegerT) -> Event value
  Drag :: (value 'IntegerT, value 'IntegerT)
       -> (value 'IntegerT, value 'IntegerT)
       -> Event value
  Button :: String -> Event value
  Update :: Event value
