module Test.IntervalArith.Dyadic where

import Prelude
import IntervalArith.Dyadic (Dyadic, fromInteger, (:^))
import IntervalArith.Misc (big, scale, toRational)
import Test.IntervalArith.Misc (ArbitraryInteger(..), ArbitraryPositiveExponent(..))
import Test.Order (totalOrderTests)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (chooseInt, sized)
import Test.Ring (commutativeRingTests)
import Test.TestUtils (SuiteEqParams1, SuiteOrdParams1, assertOpWithInput, eqWithInput)
import Test.Unit (TestSuite, suite, test)
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
            eqWithInput [ d, fromInteger (big i) ] expected result
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
            eqWithInput [ d, fromInteger (big i) ] expected result

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
            eqWithInput [ d1, d2 ] left right

dyadicOrdParams :: SuiteOrdParams1 ArbitraryDyadic Dyadic
dyadicOrdParams =
  { suitePrefix: "IntervalArith.Dyadic - <="
  , valuesName: "dyadic numbers"
  , fromArbitraryValue: \(ArbitraryDyadic d) -> d
  , leqOpWithInput: (assertOpWithInput (<=) " <= ")
  , leqOpSymbol: "<="
  , eqOpWithInput: (assertOpWithInput (==) " == ")
  , eqOpSymbol: "="
  , makeLeq: \ a b -> b
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
