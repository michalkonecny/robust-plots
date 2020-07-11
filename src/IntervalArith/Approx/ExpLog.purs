{-
  Based on CDAR.Approx fork by Michal Konečný (https://github.com/michalkonecny/cdar)
  originally developed by Jens Blanck (https://github.com/jensblanck/cdar).
-}
module IntervalArith.Approx.ExpLog where

import Prelude

import Data.Int as Int
import Data.List.Lazy as L
import Data.Maybe (Maybe(..))
import Effect.Exception.Unsafe (unsafeThrow)
import IntervalArith.Approx (Approx(..), Precision, approxAutoMB, approxMB, boundErrorTerm, boundErrorTermMB, fromInt, fromIntegerMB, lowerA, recipA, setMB, sqrA, unionA, upperA)
import IntervalArith.Approx.NumOrder ((!<!))
import IntervalArith.Approx.Taylor (taylorA)
import IntervalArith.Dyadic (atanhD, divD', ln2D, (:^))
import IntervalArith.Misc (big, integerLog2, scale)
import Math (sqrt)
import Misc.LazyList ((!!))
import Misc.LazyList.NumberSequences (factorials)

-- | Exponential by summation of Taylor series.
expTaylorA' :: Approx -> Approx
expTaylorA' Bottom = Bottom

expTaylorA' a = result
  where
  result
    | upperA a !<! zero = recipA $ aux (-a)
    | otherwise = aux a

  aux Bottom = Bottom

  aux (Approx mb m e _)
    | m == zero && e == zero = Approx mb one zero 0

  aux (Approx mb m e s)
    | e == zero =
      let
        s' = s + (integerLog2 m)

        -- r' chosen so that a' below is smaller than 1/2
        r' = Int.floor <<< sqrt <<< Int.toNumber <<< max 5 $ mb

        r = max 0 $ s' + r'

        mb'_ = mb + r + (integerLog2 m) + 1

        mb' = (120 * mb'_) `div` 100

        -- a' is a scaled by 2^k so that 2^(-r') <= a' < 2^(-r'+1)
        a' = (Approx mb' m zero (s - r))

        t = boundErrorTermMB $ taylorA mb' (map (recipA <<< fromIntegerMB mb') factorials) a'
      in
        (_ !! r) <<< L.iterate (boundErrorTermMB <<< sqrA) $ t

  aux a2 = aux (lowerA a2) `unionA` aux (upperA a2)

{-|
The exponential of an approximation. There are three implementation using
Taylor expansion here. This is just choosing between them.

More thorough benchmarking would be desirable.

Is faster for small approximations < ~2000 bits.
-}
expA :: Approx -> Approx
expA = expTaylorA'

eA :: Precision -> Approx
eA p = expA $ setMB (max 2 p) one

{- Logarithms computed by ln x = 2*atanh ((x-1)/(x+1)) after range reduction.
-}
{-|

Computing the logarithm of an approximation. This chooses the fastest implementation.

More thorough benchmarking is desirable.

Binary splitting is faster than Taylor. AGM should be used over ~1000 bits.
-}
logA :: Approx -> Maybe Approx
-- This implementation asks for the dyadic approximation of the endpoints, we
-- should instead use that, after the first range reduction, the derivative is
-- less than 3/2 on the interval, so it easy to just compute one expensive
-- computation. We could even make use of the fact that the derivative on the
-- interval x is bounded by 1/x to get a tighter bound on the error.
logA Bottom = Just Bottom

logA x@(Approx _ m e _)
  | m > e && x !<! one = Just $ -(logInternal (recipA x)) -- avoid small x
  | m > e = Just $ logInternal x
  --    let (n :^ t) = logD (negate p) $ (m-e) :^ s
  --        (n' :^ t') = logD (negate p) $ (m+e) :^ s
  --    in endToApprox (Finite ((n-1):^t)) (Finite ((n'+1):^t'))
  | m + e <= zero = Nothing -- definitely not positive
  | otherwise = Just Bottom -- possibly not positive

logInternal :: Approx -> Approx
logInternal Bottom = unsafeThrow "LogInternal: impossible"

logInternal (Approx mb m e s) =
  let
    t' = (-mb) - 10 - max 0 (integerLog2 m + s) -- (5 + size of argument) guard digits

    r = s + integerLog2 ((big 3) * m) - 1

    x = scale (m :^ s) (-r) -- 2/3 <= x' <= 4/3

    y = divD' t' (x - one) (x + one) -- so |y| <= 1/5

    (n :^ s') = flip scale 1 $ atanhD t' y

    (e' :^ s'') = divD' t' (e :^ (s - r)) x -- Estimate error term.

    res = approxMB mb n (scale (e' + one) (s'' - s')) s'
  in
    boundErrorTerm $ res + fromInt r * log2A (-t')

-- | Compute approximations of ln 2. Lifted from computation on dyadic numbers.
log2A :: Precision -> Approx
log2A p = let (m :^ s) = ln2D (-p) in approxAutoMB m one s
