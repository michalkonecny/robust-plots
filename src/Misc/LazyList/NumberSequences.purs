module Misc.LazyList.NumberSequences where

import Prelude
import Data.List.Lazy (List, (:))
import Data.List.Lazy as L
import Effect.Exception.Unsafe (unsafeThrow)
import Misc.LazyList (EmptySingletonMore(..), emptySingletonMore)

ones :: forall a. Semiring a => List a
ones = L.repeat one

onetwoetc :: forall a. Semiring a => List a
onetwoetc = L.iterate (_ + one) one

-- |Gives a list of powers of a number, i.e., [1,x,x^2,...].
powers :: forall a. Semiring a => a -> List a
powers x = L.iterate (_ * x) one

-- | A list of factorial values [1,1,2,6,24,...].
factorials :: forall a. Semiring a => List a
factorials = myscan one onetwoetc
  where
  myscan u l =
    case L.step l of
      L.Cons x xs -> u : myscan (u * x) xs
      L.Nil -> unsafeThrow "factorials: impossible"

-- test = L.take 0 (factorials :: List Int)

-- | A list of the factorial values of odd numbers.
oddFactorials :: forall a. Semiring a => List a
oddFactorials = pickOdd factorials
  where
  pickOdd xs = case emptySingletonMore xs of
    More _ x rest -> x : (pickOdd rest)
    _ -> unsafeThrow "oddFactorials: impossible"
