module Misc.Array where

import Prelude
import Data.Array (length, singleton, slice, (..))

split :: forall a. Int -> Array a -> Array (Array a)
split splits values =
  if splits > valuesCount then
    map singleton values
  else
    map splitValues (0 .. splits)
  where
  valuesCount = length values

  perSplit = valuesCount / splits

  splitValues :: Int -> Array a
  splitValues index =
    if index + perSplit - 1 > valuesCount then
      slice index (valuesCount - 1) values
    else
      slice index (index + perSplit - 1) values