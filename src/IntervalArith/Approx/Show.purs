{-
  Based on CDAR.Approx fork by Michal Konečný (https://github.com/michalkonecny/cdar)
  originally developed by Jens Blanck (https://github.com/jensblanck/cdar).
-}
module IntervalArith.Approx.Show where

import Prelude
import Data.Array (elem, reverse)
import Data.Array.Partial (head)
import Data.BigInt (BigInt, abs, even, fromInt, quot, rem, toNumber)
import Data.Char (fromCharCode)
import Data.Foldable (null)
import Data.Int (round) as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.String as Str
import Data.String.CodeUnits as StrCU
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3)
import Data.Unfoldable (replicate, unfoldr)
import Partial.Unsafe (unsafePartial)
import IntervalArith.Misc (Integer, big, shift, fromCA)
import IntervalArith.Approx.Type (Approx(..))

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
