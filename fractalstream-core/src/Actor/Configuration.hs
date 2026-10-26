{-# language OverloadedStrings #-}
module Actor.Configuration
  ( Configuration(..)
  ) where

import Actor.Layout
import Data.DynamicValue

data Configuration = Configuration
  { coTitle    :: Parsed String
  , coSize     :: Variable Dimensions
  , coContents :: Layout
  }
