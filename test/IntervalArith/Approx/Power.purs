module Test.IntervalArith.Approx.Power
  ( approxTests_PowA
  ) where

import Prelude
import Data.Maybe (fromJust)
import IntervalArith.Approx (consistent, fromInt, fromRationalPrec, lowerA, modA, recipA, sqrA)
import IntervalArith.Approx.ExpLog (eA, expA, powA)
import IntervalArith.Approx.NumOrder (absA, (!<=!))
import IntervalArith.Approx.ShowA (showA)
import IntervalArith.Approx.Sqrt (sqrtA)
import IntervalArith.Misc (two)
import Partial.Unsafe (unsafePartial)
import Test.IntervalArith.Approx.Arbitrary (ArbitraryApprox(..))
import Test.TestUtils (assertOpWithInput)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

approxTests_PowA :: TestSuite
approxTests_PowA =
  suite "IntervalArith.Approx - power" do
    test "SHOULD HOLD powA a 0 = 1 FOR ALL Approx a"
      $ quickCheck
      $ \aPre ->
          let
            -- given
            (ArbitraryApprox a) = aPre

            -- when
            result = unsafePartial $ fromJust $ powA a (fromInt 0)

            -- then
            expected = one

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show a, showA a ]
          in
            result `consistentOp` expected
    test "SHOULD HOLD powA a 2 = sqrA a FOR ALL Approx a"
      $ quickCheck
      $ \aPre ->
          let
            -- given
            (ArbitraryApprox a) = aPre

            -- when
            result = unsafePartial $ fromJust $ powA a (fromInt 2)

            -- then
            expected = sqrA a

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show a, showA a ]
          in
            result `consistentOp` expected
    test "SHOULD HOLD powA a (-1) = 1/a FOR ALL Approx a"
      $ quickCheck
      $ \aPre ->
          let
            -- given
            (ArbitraryApprox a) = aPre

            -- when
            result = unsafePartial $ fromJust $ powA a (fromInt (-1))

            -- then
            expected = recipA a

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show a, showA a ]
          in
            result `consistentOp` expected
    test "SHOULD HOLD powA a 0.5 = sqrtA a FOR ALL Approx a > 0"
      $ quickCheck
      $ \aPre ->
          let
            -- given
            (ArbitraryApprox a1) = aPre

            a2 = absA a1

            a = if a2 !<=! zero then one else a2

            -- when
            result = unsafePartial $ fromJust $ powA a (fromRationalPrec 50 (one / two))

            -- then
            expected = unsafePartial $ fromJust $ sqrtA a

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show a, showA a ]
          in
            result `consistentOp` expected
    test "SHOULD HOLD powA e a = e^a FOR ALL Approx a"
      $ quickCheck
      $ \aPre ->
          let
            -- given
            (ArbitraryApprox a1) = aPre

            -- avoid large expenonets, as they make exp take too long 
            a = (lowerA a1) `modA` (fromInt 100) - (fromInt 50)

            -- when
            result = unsafePartial $ fromJust $ powA (eA 50) a

            -- then
            expected = expA a

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show a, showA a ]
          in
            result `consistentOp` expected
