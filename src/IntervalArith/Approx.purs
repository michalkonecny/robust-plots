{-
  Based on CDAR.Approx fork by Michal Konečný (https://github.com/michalkonecny/cdar)
  originally developed by Jens Blanck (https://github.com/jensblanck/cdar).
-}
module IntervalArith.Approx where

import Prelude
import Data.Array (elem, reverse)
import Data.Array.Partial (head)
import Data.BigInt (BigInt, abs, even, fromInt, quot, rem, shl, toNumber)
import Data.Char (fromCharCode)
import Data.Foldable (intercalate, null)
import Data.Int (round, toNumber) as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.String as Str
import Data.String.CodeUnits as StrCU
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3)
import Data.Unfoldable (replicate, unfoldr)
import Partial.Unsafe (unsafePartial)

-- | A type synonym. Used to denote number of bits after binary point.
type Precision
  = Int

type Integer
  = BigInt

big :: Int -> Integer
big = fromInt

shift :: BigInt -> Int -> BigInt
shift x n = shl x (Int.toNumber n)

fromCA :: Array Char -> String
fromCA = StrCU.fromCharArray

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

Gives a decimal representation of an approximation. It tries to give as many
decimal digits as possible given the precision of the approximation. The
representation may be wrong by 1 ulp (unit in last place). If the value is not
exact the representation will be followed by @~@.

The representation is not always intuitive:

>>> showA (Approx 1 1 0)
"1.~"

The meaning of the above is that it is 1, but then the added @~@ (which must
be after the decimal point) means that the last position may be off by 1,
i.e., it could be down to 0 or up to 2. And [0,2] is indeed the range encoded
by the above approximation.
-}
showA :: Approx -> String
showA = showInBaseA 10

-- |Similar to 'showA' but can generate representations in other bases (<= 16).
{- am is the absolute value of the significand
   b corresponds to the value 1 with respect to the shift s -- this is used to find the digits in the auxiliary functions
   i is the integral part of am
   f is the fractional part of am
   i' and f' are the integral and fractional parts relevant for near zero approximations
   e' is the error term shifted appropriately when s positive, also set to at least 1
     (otherwise odd bases will yield infinite expansions)
-}
showInBaseA :: Int -> Approx -> String
showInBaseA _ Bottom = "⊥"

showInBaseA baseInt (Approx _ m e s) =
  let
    base = big baseInt
  in
    if e == big 0 && (even base || s >= 0) then
      sign <> showExactA base b i f
    else
      if am < e then
        "±" <> showNearZeroA base b i' f'
      else
        sign <> showInexactA base b i f e'
  where
  b = shift (big 1) (max 0 (-s))

  am = abs m

  i = shift am s

  e' = max (big 1) $ shift e (max 0 s)

  --   f = am .&. (b-1)
  f = chopBits am

  chopBits a = a - ((a `div` b) * b) -- a >= 0

  i' = shift (am + e) s

  --   f' = (am+e) .&. (b-1)
  f' = chopBits (am + e)

  sign = if m < (big 0) then "-" else ""

{-|
    showExactA 10 (big 10000) (big 31) (big 124) == "31.0124"
    showExactA 2 (big 1024) (big 7) (big 31) == "111.0000011111"
-}
showExactA :: Integer -> Integer -> Integer -> Integer -> String
showExactA base b i f =
  let
    g i' =
      let
        q = quot i' base

        r = rem i' base
      in
        if i' == fromInt 0 then
          Nothing
        else
          Just (Tuple (unsafeToDigit r) q)

    ip = reverse (unfoldr g i)

    h f' =
      let
        q = quot (base * f') b

        r = rem (base * f') b
      in
        if f' == fromInt 0 then
          Nothing
        else
          Just (Tuple (unsafeToDigit q) r)

    fp = unfoldr h f
  in
    (if null ip then "0" else fromCA ip)
      <> (if null fp then "" else ".")
      <> fromCA fp

showNearZeroA :: Integer -> Integer -> Integer -> Integer -> String
showNearZeroA base b i f =
  let
    s = showExactA base b i f

    t = StrCU.takeWhile (flip elem [ '0', '.', '~' ]) s

    u = StrCU.takeWhile ((/=) '.') s
  in
    if Str.null t then
      fromCA $ replicate (Str.length u) '~'
    else
      t <> "~"

showInexactA :: Integer -> Integer -> Integer -> Integer -> Integer -> String
showInexactA base b i f e =
  let
    g (Tuple n (Tuple b' (Tuple f' _)))
      | n == big 0 =
        if b' < f' + e then
          Just (Tuple '1' (tuple3 (big 0) (base * b') f'))
        else
          Nothing

    g (Tuple n (Tuple b' (Tuple f' _))) =
      let
        q = quot n base

        r = rem n base

        z = tuple3 q (base * b') (r * b' + f')
      in
        if e + f' <= b' then
          Just (Tuple (unsafeToDigit r) z)
        else
          if e <= min f' b' then
            Just (Tuple (unsafeToDigit ((r + (fromInt 1)) `rem` base)) z)
          else
            Just (Tuple '~' z)

    intRev = unfoldr g (tuple3 i b f)

    noFrac = case intRev of
      [] -> false
      _ -> unsafePartial (head intRev) == '~'

    int = if null intRev then "0" else fromCA (reverse intRev)

    h (Tuple f' err) =
      let
        q = quot (base * f') b

        r = rem (base * f') b

        err' = base * err

        z = Tuple r err'
      in
        if err' + r <= b then
          Just (Tuple (unsafeToDigit q) z)
        else
          if err' <= min r b then
            Just (Tuple (unsafeToDigit ((q + (big 1) `rem` base))) z)
          else
            Nothing

    frac = fromCA $ unfoldr h (Tuple f e)
  in
    int
      <> if noFrac then
          ""
        else
          "." <> frac <> "~"

unsafeToDigit :: BigInt -> Char
unsafeToDigit = unsafePartial toDigit

toDigit :: Partial => BigInt -> Char
toDigit nB =
  let
    n = Int.round $ toNumber nB
  in
    fromJust $ fromCharCode (n + 48)
