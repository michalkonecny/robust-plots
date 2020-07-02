module Misc.Array where

import Prelude
import Data.Array (length, singleton, slice, (..))

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
