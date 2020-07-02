module IntervalArith.Approx.Pow where

import Prelude
import Data.Array ((..))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import IntervalArith.Approx (Approx)

-- | Raises an `Approx` to a given integer power. Note that this does not handle negative powers.
powA :: Approx -> Int -> Maybe Approx
powA a n =
  if n == zero then
    Just one
  else
    if n > zero then
      Just $ foldr (*) one $ map (\_ -> a) (1 .. n)
    else
      Nothing
