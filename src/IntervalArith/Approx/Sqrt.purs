{-
  Based on CDAR.Approx fork by Michal Konečný (https://github.com/michalkonecny/cdar)
  originally developed by Jens Blanck (https://github.com/jensblanck/cdar).
-}
module IntervalArith.Approx.Sqrt where

import Prelude
import Data.BigInt as BigInt
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect.Exception.Unsafe (unsafeThrow)
import IntervalArith.Approx (Approx(..), Precision, approxAutoMB, approxMB, endToApprox, errorBits, increasingPartialFunctionViaBounds, limitAndBoundMB, mapMB, recipA, setMB, significance, unionA, upperA, upperBound)
import IntervalArith.Dyadic (shiftD, sqrtRecD, (:^))
import IntervalArith.Extended (Extended(..))
import IntervalArith.Misc (two)

{-|
Compute the square root of an approximation.

This and many other operations on approximations is just a reimplementation of
interval arithmetic, with an extra argument limiting the effort put into the
computation. This is done via the precision argument.

The resulting approximation should approximate the image of every point in the
input approximation.
-}
sqrtA :: Approx -> Maybe Approx
sqrtA Bottom = Just Bottom

sqrtA x@(Approx mb m e _)
  | m == zero && e == zero = Just x -- x = zero
  | m < -e = Nothing -- x definitely negative
  | m <= e = map (unionA zero) $ sqrtA (upperA x) -- x contains zero, ignore any negative values
  | e > zero = increasingPartialFunctionViaBounds sqrtA x -- x positive, not exact
  | otherwise = result -- e == zero, ie x is exact
    where
    k = 2 * mb + 2

    result
      | upperBound x < one = Just $ sqrtRecA k (recipA $ setMB k x)
      | otherwise = Just $ limitAndBoundMB mb $ x * sqrtRecA k x

{-|
This uses Newton's method for computing the reciprocal of the square root.
-}
sqrtRecA :: Precision -> Approx -> Approx
sqrtRecA _ Bottom = Bottom

sqrtRecA k a@(Approx mb m e s)
  | e == zero =
    let
      (n :^ t) = shiftD (-k) $ sqrtRecD (-k - 2) (m :^ s)
    in
      mapMB (max mb) $ approxAutoMB n one t
  | m == e =
    let
      (n :^ t) = sqrtRecD (s `Int.quot` 2 - errorBits) ((m + e) :^ s)

      n' = (n + two) `BigInt.quot` two
    in
      approxMB mb n' n' t
  | otherwise =
    let
      p = case significance a of
        Finite p_ -> p_
        _ -> unsafeThrow "internal error in sqrtRecA"

      s' = s `Int.quot` 2 - p - errorBits

      (n :^ t) = sqrtRecD s' ((m - e) :^ s) -- upper bound of result

      -- We have tried to use sqrtRecD' with the above value as
      -- a first approximation to the result. However, the low
      -- endpoint may be too far away as a starting value to
      -- ensure convergence to the right root. It's possible
      -- that if we swap the order we would be fine. But as it
      -- is, this computes a new first approximation.
      (n' :^ t') = sqrtRecD s' ((m + e) :^ s) -- lower bound of result
    in
      endToApprox mb (Finite ((n' - one) :^ t')) (Finite ((n + one) :^ t))
