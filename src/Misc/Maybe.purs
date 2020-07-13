module Misc.Maybe where

import Data.Maybe (Maybe(..))

-- | Maps the given `Maybe a` to `Nothing` if the value conforms to the given predicate. If the 
-- | given `Maybe a` is `Nothing` then `Nothing` is returned.
-- | ```purescript
-- | toNothingIf _ Nothing = Nothing
-- | ```
toNothingIf :: forall a. (a -> Boolean) -> Maybe a -> Maybe a
toNothingIf _ Nothing = Nothing

toNothingIf check maybeValue@(Just value) = if check value then Nothing else maybeValue
