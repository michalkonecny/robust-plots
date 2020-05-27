module Test.IntervalArith.Dyadic where

import Prelude
import IntervalArith.Dyadic (Dyadic, (:^))
import IntervalArith.Misc (scale)
import Test.IntervalArith.Misc (ArbitraryInteger(..), ArbitraryPositiveExponent(..))
import Test.Ring (commutativeRingTests)
import Test.QuickCheck (class Arbitrary, arbitrary, (==?))
import Test.QuickCheck.Gen (chooseInt, sized)
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
  suite "IntervalArith.Misc - Dyadic arithmetic" do
    dyadicTests_Scaling
    dyadicTests_Ring

dyadicTests_Scaling :: TestSuite
dyadicTests_Scaling =
  suite "IntervalArith.Misc - Dyadic scaling by powers of 2" do
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
            expected ==? result
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
            expected ==? result

dyadicTests_Ring :: TestSuite
dyadicTests_Ring =
  commutativeRingTests
    { suitePrefix: "IntervalArith.Misc - Dyadic"
    , valuesName: "dyadic numbers"
    , fromArbitraryValue: \(ArbitraryDyadic d) -> d
    , eqOp: (==?)
    , eqOpSymbol: "="
    }
