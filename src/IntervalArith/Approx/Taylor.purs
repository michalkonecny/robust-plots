{-
  Based on CDAR.Approx fork by Michal Konečný (https://github.com/michalkonecny/cdar)
  originally developed by Jens Blanck (https://github.com/jensblanck/cdar).
-}
module IntervalArith.Approx.Taylor where

import Prelude
import Data.List.Lazy (List)
import Data.List.Lazy as L
import Data.Tuple (Tuple(..), fst)
import IntervalArith.Approx (Approx, Precision, fudge, limitAndBound, nonZeroCentredA)
import Misc.LazyList (unsafeTail)
import Misc.LazyList.NumberSequences (powers)

{-|
Computes the sum of the form ∑ aₙxⁿ where aₙ and x are approximations.

Terms are added as long as they are larger than the current precision bound.
The sum is adjusted for the tail of the series. For this to be correct we need
the the terms to converge geometrically to 0 by a factor of at least 2.
-}
taylorA :: Precision -> List Approx -> Approx -> Approx
taylorA res as x = fudge sm d
  where
  addNext xs = L.zip xs (unsafeTail xs)

  sumAndNext = L.foldl aux (Tuple zero zero)
    where
    aux (Tuple a _) (Tuple b dd) = Tuple (a+b) dd

  (Tuple sm d) =
    sumAndNext
      $ L.takeWhile (nonZeroCentredA <<< fst)
      $ addNext
      $ map (limitAndBound res)
      $ L.zipWith (*) as (powers x)
