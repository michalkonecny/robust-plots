module IntervalArith.Dyadic where

import Prelude
import Data.BigInt (abs, fromNumber)
import Data.BigInt as BigInt
import Data.Foldable (sum)
import Data.Int (even, odd)
import Data.List.Lazy as L
import Data.Maybe (Maybe(..))
import Data.Ratio ((%))
import Effect.Exception.Unsafe (unsafeThrow)
import IntervalArith.Misc (class Scalable, class ToRational, Integer, big, bit, integerLog2, roundRational, scale, shift, toRational, (^), (^^))
import Math (sqrt)
import Misc.LazyList.NumberSequences (oneThreeEtc, oneTwoEtc)

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

absD :: Dyadic -> Dyadic
absD (a :^ s) = (abs a) :^ s

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
half = (one :^ -1)

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
shiftD t (m :^ s) =
  if t <= s then
    shift m (s - t) :^ t
  else
    shift (m + bit (t - s - 1)) (s - t) :^ t

divD :: Dyadic -> Dyadic -> Dyadic
divD a (n :^ t) =
  let
    (m :^ _) = shiftD (2 * t) a
  in
    roundRational (m % n) :^ t

divD' :: Int -> Dyadic -> Dyadic -> Dyadic
divD' p a b =
  let
    (m :^ _) = shiftD (2 * p) a

    (n :^ _) = shiftD p b
  in
    roundRational (m % n) :^ p

atanhD :: Int -> Dyadic -> Dyadic
atanhD t x@(a :^ s) =
  if integerLog2 (abs a) + s >= zero then
    unsafeThrow "atanhD: Argument outside domain, (-1,1)"
  else
    let
      -- number of guard digits is 5+k where k depends on the precision [how do we know if this is enough?]
      t' = t - 5 - integerLog2 (abs $ big t) `div` 2

      g _x _y = shiftD t' (_x * _y)

      x2 = g x x

      b = L.iterate (g x2) one

      c = map (divD' t' one <<< fromInteger) oneThreeEtc
    in
      shiftD t <<< g x <<< sum <<< L.takeWhile (_ /= zero) $ L.zipWith g b c

-- |Compute dyadic values close to ln 2.
ln2D :: Int -> Dyadic
ln2D t =
  let
    t' = t - 10 - 2 * (integerLog2 (abs (big t)))

    a = map ((_ :^ t') <<< roundRational <<< (bit (-t') % _)) $ L.iterate ((big 3) * _) (big 3)

    b = map ((_ :^ t') <<< roundRational <<< (bit (-t') % _)) $ L.iterate ((big 4) * _) (big 4)

    c = map ((_ :^ t') <<< roundRational <<< (bit (-t') % _)) oneTwoEtc

    d = L.zipWith (+) a b

    e = L.takeWhile (_ /= zero) <<< map (shiftD t') $ L.zipWith (*) d c
  in
    shiftD t $ sum e

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
  initSqrtD (m :^ _)
    | m == zero = zero

  initSqrtD (m :^ s) =
    let
      i = integerLog2 m

      n = shift m (2 - i)

      s' = (s + i) `div` 2 - 3
    in
      if odd (s + i) then
        (n + (big 8)) :^ s'
      else
        (n + (big 4)) :^ s'

-- |Square root with initial approximation as third argument.
sqrtD' :: Int -> Dyadic -> Dyadic -> Dyadic
sqrtD' t x@(m :^ _) y = result
  where
  result
    | m == zero = zero :^ zero
    | m > zero = convergeIterate (newtonStep x t) $ shiftD t y
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
initSqrtRecDoubleD (m :^ _)
  | m == zero = unsafeThrow "Divide by zero in initSqrtRecDoubleD"

initSqrtRecDoubleD (m :^ s) =
  let
    i = integerLog2 m

    n = shift m (52 - i)

    s' = (-i - s) `div` 2 - 52

    dbl_m' =
      (_ * (2.0 ^ 53)) <<< (1.0 / _) <<< sqrt
        $ (BigInt.toNumber n)
        * 0.5
        ^ (if even (s + i) then 52 else 51)

    m' = case fromNumber dbl_m' of
      Just m'' -> m''
      _ -> unsafeThrow "internal error in initSqrtRecDoubleD"

    t = if m' /= bit 52 && even (s + i) then s' - 1 else s'
  in
    m' :^ t

-- |Reciprocal of square root using Newton's iteration with inital value as third argument.
sqrtRecD' :: Int -> Dyadic -> Dyadic -> Dyadic
sqrtRecD' t a x0 =
  let
    step x = shiftD t $ x + shiftD t (x * (one - shiftD t (x * x * a)) * half)
  in
    convergeIterate step x0

-- Assuming a seqence of dyadic numbers with the same exponent converges
-- quadratically. Then this returns the first element where it is known that
-- the sequence will become essentially constant (differing by at most 1). If
-- the numbers contain sufficiently many bits we need only wait until the
-- difference has less than half as many bits (with a small margin).
convergeIterate :: (Dyadic -> Dyadic) -> Dyadic -> Dyadic
convergeIterate getNext x1 = converge x1 (getNext x1)
  where
  converge (m :^ s) _
    | m == zero = (zero :^ s)

  converge (m :^ _) d@(n :^ _) =
    let
      a = integerLog2 m

      b = integerLog2 (abs (m - n))
    in
      if a <= 6 then
        if abs (m - n) <= one then d else converge d (getNext d)
      else
        if 2 * b <= a - 3 then d else converge d (getNext d)
