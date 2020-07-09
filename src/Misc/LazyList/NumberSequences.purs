module Misc.LazyList.NumberSequences where

import Prelude
import Data.Int (even)
import Data.List.Lazy (List, (:))
import Data.List.Lazy as L
import Data.Tuple (fst, snd)

ones :: forall a. Semiring a => List a
ones = L.repeat one

onetwoetc :: forall a. Semiring a => List a
onetwoetc = L.iterate (_ + one) one

-- |Gives a list of powers of a number, i.e., [1,x,x^2,...].
powers :: forall a. Semiring a => a -> List a
powers x = L.iterate (_ * x) one

-- | A list of factorial values [1,1,2,6,24,...].
factorials :: forall a. Semiring a => List a
factorials = one : L.scanrLazy (*) one onetwoetc

-- test = L.take 0 (factorials :: List Int)
-- | A list of the factorial values of odd numbers.
oddFactorials :: forall a. Semiring a => List a
oddFactorials =
  map snd
    $ L.filter (even <<< fst)
    $ L.zip (onetwoetc :: List Int) factorials
