{-
  Based on CDAR.Approx fork by Michal Konečný (https://github.com/michalkonecny/cdar)
  originally developed by Jens Blanck (https://github.com/jensblanck/cdar).
-}
module IntervalArith.Approx.SinCos where

import Prelude

import Data.Int as Int
import Data.List.Lazy as L
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import IntervalArith.Approx (Approx(..), boundErrorTerm, fromInt, fromInteger, fromIntegerMB, isExact, lowerA, modA, recipA, setMB, sqrA, unionA, upperA)
import IntervalArith.Approx.NumOrder (absA, (!<=!))
import IntervalArith.Approx.Pi (piA)
import IntervalArith.Approx.Taylor (taylorA)
import IntervalArith.Misc (big, integerLog2, two, (^))
import Math (sqrt)
import Misc.LazyList ((!!))
import Misc.LazyList.NumberSequences (oddFactorials)

-- | First level of range reduction for sine. Brings it into the interval [-π/2,π/2].
sinTaylorRed1A :: Approx -> (Tuple Approx (Tuple (Maybe Approx) (Maybe Approx)))
sinTaylorRed1A Bottom = Tuple Bottom (Tuple Nothing Nothing)

sinTaylorRed1A a@(Approx mb _ _ _) =
  let
    _pi = piA (mb + 10)

    _halfPi = _pi / two

    x = setMB mb <<< (_ - _halfPi) <<< absA <<< (_pi - _) <<< absA <<< (_ - _halfPi) <<< modA a $ two * _pi

    xL = lowerA x

    xR = upperA x

    _halfPiL = lowerA _halfPi
  in
    Tuple x
      $ Tuple
          (if (-_halfPiL) !<=! xL then Just xL else Nothing) -- guarantee -π/2 <= xL
          (if xR !<=! _halfPiL then Just xR else Nothing) -- guarantee xR <= π/2

-- | Second level of range reduction for sine.
sinTaylorRed2A :: Approx -> Approx
sinTaylorRed2A Bottom = Approx 64 zero one 0 -- [-1,1]

sinTaylorRed2A a@(Approx mb m _ s) =
  let
    k = max 0 (integerLog2 m + s + (Int.floor <<< sqrt $ Int.toNumber mb))

    a' = a * recipA (fromIntegerMB mb $ (big 3) ^ k)

    a2 = negate $ sqrA a'

    t = taylorA mb (map (recipA <<< setMB mb <<< fromInteger) oddFactorials) a2

    step x = boundErrorTerm $ x * ((fromInt 3) - (fromInt 4) * sqrA x)
  in
    (_ !! k) <<< L.iterate step <<< boundErrorTerm $ t * a'

-- | Computes sine by summation of Taylor series after two levels of range reductions.
sinTaylorA :: Approx -> Approx
sinTaylorA Bottom = Approx 64 zero one 0 -- [-1,1]

sinTaylorA a@(Approx mb _ _ _) = result
  where
  (Tuple aRed (Tuple maRedL maRedR)) = sinTaylorRed1A a

  result
    | isExact aRed = sinTaylorRed2A aRed
    | otherwise = sL `unionA` sR -- aRed is in the interval [-π/2,π/2] where sine is monotone

  sL = case maRedL of
    Nothing -> Approx mb (-one) zero 0 -- aRed probably contains -pi/2
    Just aRedL -> sinTaylorRed2A aRedL

  sR = case maRedR of
    Nothing -> Approx mb one zero 0 -- aRed probably contains +pi/2
    Just aRedR -> sinTaylorRed2A aRedR

-- | Computes the sine of an approximation. Chooses the best implementation.
sinA :: Approx -> Approx
sinA = sinTaylorA

-- | Computes the cosine of an approximation. Chooses the best implementation.
cosA :: Approx -> Approx
cosA Bottom = Approx 64 zero one 0 -- [-1,1]

cosA x@(Approx mb _ _ _) = sinA ((Approx 1 one zero (-1)) * piA (mb + 2) - x)

tanA :: Approx -> Approx
tanA x = (sinA x) / (cosA x)

