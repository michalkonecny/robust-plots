module Misc.Array where

import Prelude
import Data.Array (length, singleton, slice, (..))

-- | Seperates an array into a specified number of arrays. If the specified number of sub arrays is 
-- | larger than the number of elements then the array of sub-arrays will be split into singleton 
-- | arrays, one for each element.
split :: forall a. Int -> Array a -> Array (Array a)
split splits values =
  if splits >= valuesCount then
    map singleton values
  else
    map splitValues $ 0 .. (splits - 1)
  where
  valuesCount = length values

  perSplit = valuesCount / splits

  splitValues :: Int -> Array a
  splitValues index =
    if i + perSplit > valuesCount || index == splits - 1 then
      slice i valuesCount values
    else
      slice i (i + perSplit) values
    where
    i = index * perSplit
