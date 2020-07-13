module Test.IntervalArith.Approx.ExpLog
  ( approxTests_ExpLogA
  ) where

import Prelude
import Data.Maybe (fromJust)
import IntervalArith.Approx (consistent, fromInt, lowerA, modA, setMB)
import IntervalArith.Approx.ExpLog (eA, expA, logA)
import IntervalArith.Approx.NumOrder (absA, (!<=!))
import IntervalArith.Approx.ShowA (showA)
import Partial.Unsafe (unsafePartial)
import Test.IntervalArith.Approx.Arbitrary (ArbitraryApprox(..))
import Test.IntervalArith.Misc (ArbitraryInt0To1000(..))
import Test.TestUtils (assertOpWithInput)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

approxTests_ExpLogA :: TestSuite
approxTests_ExpLogA =
  suite "IntervalArith.Approx - exp and log" do
    test "SHOULD HOLD expA 1 = e"
      $ quickCheck
      $ \pPre ->
          let
            (ArbitraryInt0To1000 p) = pPre

            -- given
            input1 = setMB p one

            -- when
            result = expA input1

            -- then
            expected = eA p

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show p, show input1, showA input1 ]
          in
            result `consistentOp` expected
    test "SHOULD HOLD expA a * expA b = expA (a + b) FOR ANY Approx a b"
      $ quickCheck
      $ \aPre bPre ->
          let
            -- given
            (ArbitraryApprox a1) = aPre

            (ArbitraryApprox b1) = bPre

            -- avoid large expenonets, as they make exp take too long 
            a = (lowerA a1) `modA` (fromInt 100) - (fromInt 50)

            b = (lowerA b1) `modA` (fromInt 100) - (fromInt 50)

            -- when
            result = expA a * expA b

            -- then
            expected = expA (a + b)

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show a, showA a, show b, showA b ]
          in
            result `consistentOp` expected
    test "SHOULD HOLD expA (log a) = a FOR ANY Approx a"
      $ quickCheck
      $ \aPre ->
          let
            -- given
            (ArbitraryApprox a1) = aPre

            a2 = absA a1

            a = if a2 !<=! zero then one else a2

            -- when
            result = expA (unsafePartial $ fromJust $ logA a)

            -- then
            expected = a

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show a, showA a ]
          in
            result `consistentOp` expected
