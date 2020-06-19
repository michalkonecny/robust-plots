module IntervalArith.Dyadic where

import Prelude

import Data.BigInt (abs)
import Data.BigInt as BigInt
import Data.Int (even, odd, round)
import Data.Ratio ((%))
import Effect.Exception.Unsafe (unsafeThrow)
import IntervalArith.Misc (class Scalable, class ToRational, Integer, big, bit, integerLog2, roundRational, scale, shift, toRational, (^), (^^))
import Math (sqrt)

-- |The Dyadic data type.
data Dyadic
  = Dyadic Integer Int

infix 8 Dyadic as :^

instance showDyadic :: Show Dyadic where
  show (Dyadic a s) = "(Dyadic " <> show a <> " " <> show s <> ")"

instance eqDyadic :: Eq Dyadic where
  eq (a :^ s) (b :^ t)
    | s <= t = a == shift b (t - s)
    | otherwise = shift a (s - t) == b

instance ordDyadic :: Ord Dyadic where
  compare (a :^ s) (b :^ t)
    | s <= t = compare a (shift b (t - s))
    | otherwise = compare (shift a (s - t)) b

instance scalableDyadic :: Scalable Dyadic where
  scale (a :^ s) n = a :^ (s + n)

instance semiringDyadic :: Semiring Dyadic where
  zero = (zero :^ 0)
  one = (one :^ 0)
  add (a :^ s) (b :^ t)
    | s <= t = (a + scale b (t - s)) :^ s
    | otherwise = (scale a (s - t) + b) :^ t
  mul (a :^ s) (b :^ t) = (a * b) :^ (s + t)

half :: Dyadic
half = (one:^ -1)

instance ringDyadic :: Ring Dyadic where
  sub (a :^ s) (b :^ t)
    | s <= t = (a - scale b (t - s)) :^ s
    | otherwise = (scale a (s - t) - b) :^ t

instance commutativeRingDyadic :: CommutativeRing Dyadic

fromInteger :: Integer -> Dyadic
fromInteger i = i :^ 0

fromInt :: Int -> Dyadic
fromInt i = (big i) :^ 0

instance toRationalDyadic :: ToRational Dyadic where
  toRational (a :^ s) = (toRational a) * (toRational 2) ^^ s

dyadicToNumber :: Dyadic -> Number
dyadicToNumber (a :^ s) = (BigInt.toNumber a) * (2.0) ^^ s

-- | Shift a dyadic number to a given base and round in case of right shifts.
shiftD :: Int -> Dyadic -> Dyadic
shiftD t (m:^s) =
    if t <= s
    then shift m (s-t) :^ t
    else shift (m + bit (t-s-1)) (s-t) :^ t

-- Square root

divD :: Dyadic -> Dyadic -> Dyadic
divD a (n:^t) = let (m:^_) = shiftD (2*t) a
                 in roundRational (m % n) :^ t

{- |Computes the square root of a Dyadic number to the specified base. The
   Newton-Raphson method may overestimates the square root, but the
   overestimate is bounded by 1 ulp. For example, sqrtD 0 2 will give 2,
   whereas the closest integer to the square root is 1. Need double precision
   to guarantee correct rounding, which was not considered worthwhile.

   This is actually Heron's method and is no longer used in Approx as it is
   faster to use sqrtRecD.
-}
sqrtD :: Int -> Dyadic -> Dyadic
sqrtD t x = sqrtD' t x $ initSqrtD x
    where
      -- Initial approximation of square root to about 3 bits
      initSqrtD :: Dyadic -> Dyadic
      initSqrtD (m:^_) | m == zero = zero
      initSqrtD (m:^s) = let i = integerLog2 m
                             n = shift m (2-i)
                             s' = (s+i) `div` 2 - 3
                         in if odd (s+i)
                            then (n+(big 8)):^s'
                            else (n+(big 4)):^s'

-- |Square root with initial approximation as third argument.
sqrtD' :: Int -> Dyadic -> Dyadic -> Dyadic
sqrtD' t x@(m:^_) y = result
    where
    result
      | m == zero = zero:^zero
      | m > zero  = convergeIterate (newtonStep x t) $ shiftD t y
      | otherwise = unsafeThrow "Attempting sqrt of negative dyadic number."
    -- One step of Newton iteration to find square root.
    -- The base of a need to be the same as t', and the result will have base t'.
    newtonStep :: Dyadic -> Int -> Dyadic -> Dyadic
    newtonStep d t' a = shiftD t' $ half * (a + divD d a)

-- |Reciprocal of square root using Newton's iteration.
sqrtRecD :: Int -> Dyadic -> Dyadic
sqrtRecD t a = sqrtRecD' t a $ initSqrtRecDoubleD a

-- Using Double to find a good first approximation to SqrtRec. Extracts 52
-- bits from the dyadic number and encodes a float in the range [1/2,2).
-- Decodes the result of the Double computation as a dyadic taking care of the
-- exponent explicitly (as the range of Double exponents is too small).
initSqrtRecDoubleD :: Dyadic -> Dyadic
initSqrtRecDoubleD (m :^ _) | m == zero = unsafeThrow "Divide by zero in initSqrtRecDoubleD"
initSqrtRecDoubleD (m :^ s) =
  let i = integerLog2 m
      n = shift m (52-i)
      s' = (-i-s) `div` 2 - 52
      m' = big <<< round <<< (_ * (2.0^53)) <<< (1.0/_) <<< sqrt $ 
              (BigInt.toNumber n) * 0.5 ^ (if even (s+i) then 52 else 51)
      t = if m' /= bit 52 && even (s+i) then s'-1 else s'
  in m' :^ t

-- |Reciprocal of square root using Newton's iteration with inital value as third argument.
sqrtRecD' :: Int -> Dyadic -> Dyadic -> Dyadic
sqrtRecD' t a x0 =
  let step x = shiftD t $ x + shiftD t (x * (one - shiftD t (x * x * a)) * half)
  in convergeIterate step x0


-- Assuming a seqence of dyadic numbers with the same exponent converges
-- quadratically. Then this returns the first element where it is known that
-- the sequence will become essentially constant (differing by at most 1). If
-- the numbers contain sufficiently many bits we need only wait until the
-- difference has less than half as many bits (with a small margin).
convergeIterate :: (Dyadic -> Dyadic) -> Dyadic -> Dyadic
convergeIterate getNext x1 = converge x1 (getNext x1)
  where
  converge (m:^s) _ | m == zero = (zero:^s)
  converge (m:^_) d@(n:^_) =
    let a = integerLog2 m
        b = integerLog2 (abs (m - n))
    in if a <= 6
      then if abs (m-n) <= one then d else converge d (getNext d)
      else if 2 * b <= a - 3 then d else converge d (getNext d)
