module Misc.Maybe where

import Data.Maybe (Maybe(..))

toNothingIf :: forall a. (a -> Boolean) -> Maybe a -> Maybe a
toNothingIf _ Nothing = Nothing

toNothingIf check m@(Just v) = if check v then m else Nothing
