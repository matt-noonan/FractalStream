
module Utilities
  ( sort
  , toMultiset
  , fromMultiset
  , groupsOf
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

sort :: Ord a => [a] -> [a]
sort = fromMultiset . toMultiset

toMultiset :: Ord a => [a] -> Map a Int
toMultiset = Map.fromListWith (+) . (`zip` repeat 1)

fromMultiset :: Ord a => Map a Int -> [a]
fromMultiset = concatMap (uncurry (flip replicate')) . Map.toList
  where replicate' n = replicate (abs n)

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
