module Test.IntervalArith.Dyadic where

import Prelude
import IntervalArith.Dyadic (Dyadic, dyadicToNumber, fromInt, fromInteger, (:^))
import IntervalArith.Misc (big, scale, toRational, (^))
import Test.IntervalArith.Misc (ArbitraryInteger(..), ArbitraryPositiveExponent(..))
import Test.Order (totalOrderTests)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (chooseInt, sized)
import Test.Ring (commutativeRingTests)
import Test.TestUtils (SuiteEqParams1, SuiteOrdParams1, assertOpWithInput, eqWithInput, leqWithInput)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

newtype ArbitraryDyadic
  = ArbitraryDyadic Dyadic

instance arbitraryDyadic :: Arbitrary ArbitraryDyadic where
  arbitrary =
    sized \size -> do
      (ArbitraryInteger a) <- arbitrary
      s <- chooseInt (-2 * size) (2 * size)
      pure $ ArbitraryDyadic $ a :^ s

dyadicTests :: TestSuite
dyadicTests =
  suite "IntervalArith.Dyadic - arithmetic" do
    dyadicTests_Order
    dyadicTests_Ring
    dyadicTests_Scaling
    dyadicTests_ToRational
    dyadicTests_ToNumber

dyadicTests_Scaling :: TestSuite
dyadicTests_Scaling =
  suite "IntervalArith.Dyadic - scaling by powers of 2" do
    test "SHOULD HOLD n = scale (scale n i) (-i) FOR ALL integers n and i>=0"
      $ quickCheck \dPre iPre ->
          let
            -- given
            (ArbitraryDyadic d) = dPre :: ArbitraryDyadic

            (ArbitraryPositiveExponent i) = iPre

            -- when
            result = scale (scale d i) (-i)

            -- then
            expected = d
          in
            eqWithInput [ show d, show i ] expected result
    test "SHOULD HOLD n = scale (scale n (-i)) i FOR ALL integers n and i>=0"
      $ quickCheck \dPre iPre ->
          let
            -- given
            (ArbitraryDyadic d) = dPre :: ArbitraryDyadic

            (ArbitraryPositiveExponent i) = iPre

            -- when
            result = scale (scale d (-i)) i

            -- then
            expected = d
          in
            eqWithInput [ show d, show i ] expected result

dyadicTests_Order :: TestSuite
dyadicTests_Order = totalOrderTests dyadicOrdParams

dyadicTests_ToRational :: TestSuite
dyadicTests_ToRational =
  suite "IntervalArith.Dyadic - conversion to Rational" do
    test "SHOULD HOLD Q(d1 + d2) = Q(d1) + Q(d2) FOR ALL dyadic numbers d1, d2"
      $ quickCheck \d1Pre d2Pre ->
          let
            -- given
            (ArbitraryDyadic d1) = d1Pre :: ArbitraryDyadic

            (ArbitraryDyadic d2) = d2Pre :: ArbitraryDyadic

            -- when
            -- then
            left = (toRational (d1 + d2))

            right = (toRational d1) + (toRational d2)
          in
            eqWithInput [ show d1, show d2 ] left right

dyadicTests_ToNumber :: TestSuite
dyadicTests_ToNumber =
  suite "IntervalArith.Dyadic - conversion to Number" do
    test "SHOULD GET 1.0 WHEN converting dyadic 1 to Number" do
      let
        -- given
        input = fromInt 1

        -- when
        result = dyadicToNumber input

        -- then
        expected = 1.0
      equal expected result
    test "SHOULD GET Infinity WHEN converting dyadic 10^400 to Number" do
      let
        -- given
        input = fromInteger ((big 10) ^ 400)

        -- when
        result = dyadicToNumber input

        -- then
        expected = 1.0 / 0.0
      equal expected result
    test "SHOULD HOLD d1 <= d2 ==> N(d1) <= N(d2) FOR ALL dyadic numbers d1, d2"
      $ quickCheck \d1Pre d2Pre ->
          let
            -- given
            (ArbitraryDyadic d1) = d1Pre :: ArbitraryDyadic

            (ArbitraryDyadic d2) = d2Pre :: ArbitraryDyadic

            d1' = min d1 d2

            d2' = max d1 d2

            -- when
            d1N = dyadicToNumber d1'

            d2N = dyadicToNumber d2'
          -- then
          in
            leqWithInput [ show d1', show d2' ] d1N d2N

dyadicOrdParams :: SuiteOrdParams1 ArbitraryDyadic Dyadic
dyadicOrdParams =
  { suitePrefix: "IntervalArith.Dyadic - <="
  , valuesName: "dyadic numbers"
  , fromArbitraryValue: \(ArbitraryDyadic d) -> d
  , leqOpWithInput: (assertOpWithInput (<=) " <= ")
  , leqOpSymbol: "<="
  , eqOpWithInput: (assertOpWithInput (==) " == ")
  , eqOpSymbol: "="
  , makeLeq: \a b -> b
  }

dyadicTests_Ring :: TestSuite
dyadicTests_Ring = commutativeRingTests dyadicEqParams

dyadicEqParams :: SuiteEqParams1 ArbitraryDyadic Dyadic
dyadicEqParams =
  { suitePrefix: "IntervalArith.Dyadic -"
  , valuesName: "dyadic numbers"
  , fromArbitraryValue: \(ArbitraryDyadic d) -> d
  , eqOpWithInput: (assertOpWithInput (==) " == ")
  , eqOpSymbol: "="
  }
