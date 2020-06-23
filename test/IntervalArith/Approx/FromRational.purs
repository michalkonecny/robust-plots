module Test.IntervalArith.Approx.FromRational
  ( approxTests_fromRational, approxTests_fromRationalBounds
  ) where

import Prelude
import Data.NonEmpty (foldl1, (:|))
import Data.Tuple (Tuple(..))
import IntervalArith.Approx (Approx(..), boundsR, consistent, fromRationalBoundsPrec, fromRationalPrec)
import IntervalArith.Extended (Extended(..))
import Test.IntervalArith.Misc (ArbitraryPositiveExponent(..), ArbitraryRational(..))
import Test.QuickCheck.Combinators ((&=&))
import Test.TestUtils (assertOp, assertOpWithInput)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

approxTests_fromRational :: TestSuite
approxTests_fromRational =
  suite "IntervalArith.Approx - conversion from Rational" do
    test "SHOULD HOLD addition on Approx is consistent with addition on Rational"
      $ quickCheck \aPre bPre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational b) = bPre

            (ArbitraryRational a) = aPre

            -- when
            resultAddConvert = fromRationalPrec prec (a + b)

            resultConvertAdd = (fromRationalPrec prec a) + (fromRationalPrec prec b)

            -- then
            consistentOp = assertOp consistent " `consistent` "
          in
            resultAddConvert `consistentOp` resultConvertAdd
    test "SHOULD HOLD multiplication on Approx is consistent with multiplication on Rational"
      $ quickCheck \aPre bPre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational b) = bPre

            (ArbitraryRational a) = aPre

            -- when
            resultMulConvert = fromRationalPrec prec (a * b)

            resultConvertMul = (fromRationalPrec prec a) * (fromRationalPrec prec b)

            -- then
            consistentOp = assertOp consistent " `consistent` "
          in
            resultMulConvert `consistentOp` resultConvertMul
    test "SHOULD HOLD division on Approx is consistent with division on Rational"
      $ quickCheck \aPre bPre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational b) = bPre

            (ArbitraryRational a) = aPre

            -- when
            resultDivConvert =
              if b == zero then
                Bottom
              else
                fromRationalPrec prec (a / b)

            resultConvertDiv = (fromRationalPrec prec a) / (fromRationalPrec prec b)

            -- then
            consistentOp = assertOp consistent " `consistent` "
          in
            resultDivConvert `consistentOp` resultConvertDiv

approxTests_fromRationalBounds :: TestSuite
approxTests_fromRationalBounds =
  suite "IntervalArith.Approx - conversion from Rational bounds" do
    test "SHOULD HOLD Approx from bounds is consistent with each bound"
      $ quickCheck \a1Pre a2Pre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational a1) = a1Pre

            (ArbitraryRational a2) = a2Pre

            aL = min a1 a2

            aU = max a1 a2

            -- when
            hull = fromRationalBoundsPrec prec aL aU

            Tuple hullL hullU = boundsR hull

            -- then
            leqOp = assertOp (<=) " <= "
          in
            hullL `leqOp` (Finite aL) &=& (Finite aU) `leqOp` hullU
    test "SHOULD HOLD addition on Approx is consistent with addition on Rational intervals"
      $ quickCheck \a1Pre a2Pre b1Pre b2Pre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational b1) = b1Pre

            (ArbitraryRational b2) = b2Pre

            bL = min b1 b2

            bU = max b1 b2

            (ArbitraryRational a1) = a1Pre

            (ArbitraryRational a2) = a2Pre

            aL = min a1 a2

            aU = max a1 a2

            -- when
            a = fromRationalBoundsPrec prec aL aU

            b = fromRationalBoundsPrec prec bL bU

            Tuple resultL resultU = boundsR $ a + b

            -- then
            leqOp = assertOpWithInput (<=) " <= " $ map show [ aL, aU, bL, bU ]
          in
            resultL `leqOp` (Finite $ aL + bL) &=& (Finite $ aU + bU) `leqOp` resultU
    test "SHOULD HOLD multiplication on Approx is multiplication on Rational intervals"
      $ quickCheck \a1Pre a2Pre b1Pre b2Pre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational b1) = b1Pre

            (ArbitraryRational b2) = b2Pre

            bL = min b1 b2

            bU = max b1 b2

            (ArbitraryRational a1) = a1Pre

            (ArbitraryRational a2) = a2Pre

            aL = min a1 a2

            aU = max a1 a2

            -- when
            a = fromRationalBoundsPrec prec aL aU

            b = fromRationalBoundsPrec prec bL bU

            Tuple resultL resultU = boundsR $ a * b

            products = (aL * bL) :| [ aL * bU, aU * bL, aU * bU ]

            abL = foldl1 min products

            abU = foldl1 max products

            -- then
            leqOp = assertOpWithInput (<=) " <= " $ map show [ aL, aU, bL, bU ]
          in
            resultL `leqOp` (Finite $ abL) &=& (Finite $ abU) `leqOp` resultU
    test "SHOULD HOLD recip on Approx is recip on Rational intervals"
      $ quickCheck \b1Pre b2Pre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational b1) = b1Pre

            (ArbitraryRational b2) = b2Pre

            bL = min b1 b2

            bU = max b1 b2

            b = fromRationalBoundsPrec prec bL bU

            -- when
            Tuple resultL resultU = boundsR $ recip b

            hasZero = bL <= zero && zero <= bU

            bLinv = if hasZero then PosInf else Finite (one / bL)

            bUinv = if hasZero then NegInf else Finite (one / bU)

            -- then
            leqOp = assertOpWithInput (<=) " <= " $ [ show bL, show bU, show prec ]
          in
            resultL `leqOp` bUinv &=& bLinv `leqOp` resultU
