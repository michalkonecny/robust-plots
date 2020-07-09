module Misc.LazyList.NumberSequences where

import Prelude
import Data.List.Lazy (List)
import Data.List.Lazy as L

ones :: forall a. Semiring a => List a
ones = L.repeat one

onetwoetc :: forall a. Semiring a => List a
onetwoetc = L.iterate (_ + one) one

-- |Gives a list of powers of a number, i.e., [1,x,x^2,...].
powers :: forall a. Semiring a => a -> List a
powers x = L.iterate (_ * x) one
