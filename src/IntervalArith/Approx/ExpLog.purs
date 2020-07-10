{-
  Based on CDAR.Approx fork by Michal Konečný (https://github.com/michalkonecny/cdar)
  originally developed by Jens Blanck (https://github.com/jensblanck/cdar).
-}
module IntervalArith.Approx.ExpLog where

import Prelude
import Data.Int as Int
import Data.List.Lazy as L
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import IntervalArith.Approx (Approx(..), Precision, boundErrorTerm, boundErrorTermMB, fromInt, fromInteger, fromIntegerMB, lowerA, modA, recipA, setMB, sqrA, unionA, upperA)
import IntervalArith.Approx.NumOrder (absA, (!<!), (!<=!))
import IntervalArith.Approx.Pi (piA)
import IntervalArith.Approx.Taylor (taylorA)
import IntervalArith.Misc (big, integerLog2, two, (^))
import Math (sqrt)
import Misc.LazyList ((!!))
import Misc.LazyList.NumberSequences (factorials, oddFactorials)

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
