module Test.IntervalArith.Approx.Abs
  ( approxTests_AbsA
  ) where

import Prelude
import Data.BigInt (abs)
import IntervalArith.Approx (Approx(..), consistent, fromInteger)
import IntervalArith.Approx.NumOrder (absA, (!<=!))
import IntervalArith.Misc (two)
import Test.IntervalArith.Approx.Arbitrary (ArbitraryApprox(..))
import Test.IntervalArith.Misc (ArbitraryInteger(..))
import Test.TestUtils (assertOp, assertOpWithInput)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

approxTests_AbsA :: TestSuite
approxTests_AbsA =
  suite "IntervalArith.Approx - absolute value" do
    test "SHOULD HOLD |[0±2]| = [1±1]" do
      let
        -- given
        input = Approx 10 zero two 0

        -- when
        result = absA input

        -- then
        expected = Approx 10 two two (-1)
      equal expected result
    test "SHOULD HOLD |fromInteger n| = fromInteger |n|"
      $ quickCheck \nPre ->
          let
            -- given
            (ArbitraryInteger n) = nPre

            -- when
            resultAbsConvert = fromInteger (abs n)

            resultConvertAbs = absA (fromInteger n)

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show n ]
          in
            resultAbsConvert `consistentOp` resultConvertAbs
    test "SHOULD HOLD 0 !<=! |a| FOR ALL Approx a"
      $ quickCheck \aPre ->
          let
            -- given
            (ArbitraryApprox a) = aPre

            -- when
            result = absA a

            -- then
            leqOp = assertOp (!<=!) " !<=! "
          in
            zero `leqOp` result
