{-
  Based on CDAR.Approx fork by Michal Konečný (https://github.com/michalkonecny/cdar)
  originally developed by Jens Blanck (https://github.com/jensblanck/cdar).
-}
module IntervalArith.Approx where

import Prelude
import Data.BigInt (abs)
import Data.Foldable (intercalate)
import Effect.Exception.Unsafe (unsafeThrow)
import FFI.BigInt (bitLength)
import IntervalArith.Misc (Integer, big, shift)

-- | A type synonym. Used to denote number of bits after binary point.
type Precision
  = Int

{-|
= Centred Dyadic Approximations
There are two constructors for approximations:

- 'Approx' is encodes some finite interval with dyadic endpoints. A real
  number is /approximated/ by the approximation is it belongs to the interval.
- 'Bottom' is the trivial approximation that approximates all real numbers.

The four fields of an @Approx m e s@ should be thought of as:

[@mb@] the midpoint bound, ie maximum bits available for the midpoint
[@m@] the midpoint
[@e@] the error term
[@s@] the exponent

Thus, a value @Approx p m e s@ is to be interpreted as the interval
[(m-e)*2^s, (m+e)*2^s] where |m| <= 2^p.

== Centred intervals
We have opted for a centred representation of the intervals. It is also
possible to represent the endpoints as 'Dyadic' numbers. The rationale for a
centred repersentation is that we often normalise an approximation @Approx p m e
s@ so that @e@ is limited in size. This allows many operations to only work on
one large number @m@.

== Potential for overflow
Since the last field (the exponent) is only an 'Int' it may overflow. This is
an optimisation that was adopted since it seems unlikely that overflow in a 64
bit Int exponent would occur. In a 32 bit system, this is potentially an
issue.

The 'Integer' data type is unbonded, but is, of course, bounded by the
available memory available in the computer. No attempt has been made to check
for exhausted memory.

== Approximations as a Domain

Ordered by reverse inclusion the dyadic intervals encoded by the 'Approx'
approximations (including 'Bottom') constitute the compact elements of a Scott
domain /D/. (It is a substructure of the (algebraic) interval domain.)
We will identify our approximations with the compact elements of /D/.

Increasing sequences in /D/ have suprema. A sequence /converges/ if the length
of the approximations tend to zero. The supremum of a converging sequence is a
singleton set containing a real number. Let ρ be the map taking a converging
sequence to the unique real number in the supremum. The computations on
(computable) real numbers is via this representation map ρ.

There is no check that the sequences we have are in fact increasing, but we
are assuming that all sequences are pairwise consistent. We can thus create an
increasing sequence by considering the sequence of finite suprema. For
correctness, we have to ensure that all operations done on consistent
sequences result in consistent sequences. If non-consistent sequences are
somehow input we can make no guarantees at all about the computed value.

Note, that we cannot ensure that converging sequences are mapped to converging
sequences because of properties of computable real arithmetic. In particular,
at any discuntinuity, it is impossible to compute a converging sequence.
-}
data Approx
  = Approx Int Integer Integer Int
  | Bottom

instance approxShow :: Show Approx where
  show (Approx b m e s) = intercalate " " [ "Approx", show b, "(", show m, ") (", show e, ")", show s ]
  show Bottom = "Bottom"

{-|
    Give the "bound on midpoint bit-size" component of an 'Approx'.

    The midpoint coponent should always be bounded by this as follows:
    @ abs m <= 2^mb@.
-}
mBound :: Approx -> Int
mBound (Approx mb _ _ _) = mb

mBound Bottom = unsafeThrow "mBound Bottom"

approxAutoMB :: Integer -> Integer -> Int -> Approx
approxAutoMB m e s = Approx mb m e s
  where
  ame = (abs m) + e

  mb
    | ame <= big 4 = 2
    | otherwise = bitLength ame

enforceMB :: Approx -> Approx
enforceMB Bottom = Bottom

enforceMB a@(Approx mb m e s) =
  let
    result
      | m_size <= mb = a
      | abs m <= (big 1) = a
      | otherwise = Approx mb m' e'' (s + d)

    m_size = bitLength m -- |m| <= 2^m_size

    d = m_size - mb

    m' = shift m (-d) -- we have: m' * 2^d <= m

    e' = (big 1) + (shift (e - (big 1)) (-d)) -- we have: 0 <= e <= e' * 2^d

    e''
      | m == shift m' d = e' -- no loss of information
      | otherwise = (big 1) + e'
  in
    result

approxMB :: Int -> Integer -> Integer -> Int -> Approx
approxMB mb m e s = enforceMB $ Approx mb m e s

approxMB2 :: Int -> Int -> Integer -> Integer -> Int -> Approx
approxMB2 mb1 mb2 m e s = enforceMB $ Approx (mb1 `max` mb2) m e s

mapMB :: (Int -> Int) -> Approx -> Approx
mapMB f (Approx mb m e s) = approxMB (f mb) m e s

mapMB _f Bottom = Bottom

setMB :: Int -> Approx -> Approx
setMB mb = mapMB (const mb)

-- -- |Construct a centred approximation from the end-points.
-- endToApprox :: Int -> Extended Dyadic -> Extended Dyadic -> Approx
-- endToApprox mb (Finite l) (Finite u)
--   | u < l = Bottom -- Might be better with a signalling error.
--   | otherwise =
--     let a@(m:^s) = scale (l + u) (-1)
--         (n:^t)   = u-a
--         r        = min s t
--         m'       = unsafeShiftL m (s-r)
--         n'       = unsafeShiftL n (t-r)
--     in (approxMB mb m' n' r)
-- endToApprox _ _ _ = Bottom
-- -- Interval operations
-- -- |Gives the lower bound of an approximation as an 'Extended' 'Dyadic' number.
-- lowerBound :: Approx -> Extended Dyadic
-- lowerBound (Approx _ m e s) = Finite ((m-e):^s)
-- lowerBound Bottom = NegInf
-- -- |Gives the upper bound of an approximation as an 'Extended' 'Dyadic' number.
-- upperBound :: Approx -> Extended Dyadic
-- upperBound (Approx _ m e s) = Finite ((m+e):^s)
-- upperBound Bottom = PosInf
-- |Gives the lower bound of an 'Approx' as an exact 'Approx'.
lowerA :: Approx -> Approx
lowerA Bottom = Bottom

lowerA (Approx mb m e s) = Approx mb (m - e) (big 0) s

-- |Gives the upper bound of an 'Approx' as an exact 'Approx'.
upperA :: Approx -> Approx
upperA Bottom = Bottom

upperA (Approx mb m e s) = Approx mb (m + e) (big 0) s

-- -- |Gives the mid-point of an approximation as a 'Maybe' 'Dyadic' number.
-- centre :: Approx -> Maybe Dyadic
-- centre (Approx _ m _ s) = Just (m:^s)
-- centre _ = Nothing
-- |Gives the centre of an 'Approx' as an exact 'Approx'.
centreA :: Approx -> Approx
centreA Bottom = Bottom

centreA (Approx mb m _e s) = Approx mb m (big 0) s

-- -- |Gives the radius of an approximation as a 'Dyadic' number. Currently a
-- -- partial function. Should be made to return an 'Extended' 'Dyadic'.
-- radius :: Approx -> Extended Dyadic
-- radius (Approx _ _ e s) = Finite (e:^s)
-- radius _ = PosInf
-- -- |Gives the lower bound of an approximation as an 'Extended' 'Dyadic' number.
-- diameter :: Approx -> Extended Dyadic
-- diameter (Approx _ _ e s) = Finite $ 2 * (e:^s)
-- diameter _ = PosInf
-- |Returns 'True' if the approximation is exact, i.e., it's diameter is 0.
exact :: Approx -> Boolean
exact (Approx _ _ e _) = e == (big 0)

exact Bottom = false

-- -- |Checks if a number is approximated by an approximation, i.e., if it
-- -- belongs to the interval encoded by the approximation.
-- -- approximatedBy :: Real a => a -> Approx -> Bool
-- _ `approximatedBy` Bottom = True
-- r `approximatedBy` d =
--     let r' = toRational r
--     in toRational (lowerBound d) <= r' && r' <= toRational (upperBound d)
-- -- |A partial order on approximations. The first approximation is better than
-- -- the second if it is a sub-interval of the second.
-- better :: Approx -> Approx -> Bool
-- d `better` e = lowerBound d >= lowerBound e &&
--                upperBound d <= upperBound e
-- -- |Turns a 'Dyadic' number into an exact approximation.
-- fromDyadic :: Dyadic -> Approx
-- fromDyadic (m:^s) = approxAutoMB m 0 s
-- -- |Turns a 'Dyadic' number into an exact approximation.
-- fromDyadicMB :: Int -> Dyadic -> Approx
-- fromDyadicMB mb (m:^s) = approxMB mb m 0 s
