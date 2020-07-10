module Test.IntervalArith.Approx.SinCos
  ( approxTests_SinCosA
  ) where

import Prelude
import IntervalArith.Approx (consistent, fromInt, mBound)
import IntervalArith.Approx.Pi (piA)
import IntervalArith.Approx.ShowA (showA)
import IntervalArith.Approx.SinCos (cosA, sinA)
import IntervalArith.Misc (two, (^))
import Test.IntervalArith.Approx.Arbitrary (ArbitraryApprox(..))
import Test.IntervalArith.Misc (ArbitraryInt0To1000(..))
import Test.TestUtils (assertOpWithInput)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

approxTests_SinCosA :: TestSuite
approxTests_SinCosA =
  suite "IntervalArith.Approx - sine and cosine" do
    test "SHOULD HOLD sinA (k*pi) = 0 FOR ANY integer k"
      $ quickCheck
      $ \pPre kPre ->
          let
            (ArbitraryInt0To1000 p) = pPre

            (ArbitraryInt0To1000 k2) = kPre

            k = if k2 > 500 then k2 - 1000 else k2

            -- given
            input1 = (piA p) * (fromInt k)

            -- when
            result = sinA input1

            -- then
            expected = zero

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show p, show k, show input1, showA input1 ]
          in
            result `consistentOp` expected
    test "SHOULD HOLD sinA (pi/2) = 1"
      $ quickCheck
      $ \pPre ->
          let
            (ArbitraryInt0To1000 p) = pPre

            -- given
            input1 = (piA p) / two

            -- when
            result = sinA input1

            -- then
            expected = one

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show p, show input1, showA input1 ]
          in
            result `consistentOp` expected
    test "SHOULD HOLD sinA (a + pi/2) = cos(a) FOR ALL approx a"
      $ quickCheck \aPre ->
          let
            -- given
            (ArbitraryApprox a) = aPre

            piHalf = (piA (mBound a)) / two

            -- when
            result = sinA (a + piHalf)

            -- then
            expected = cosA a

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show a, showA a ]
          in
            result `consistentOp` expected
    test "SHOULD HOLD (sin a)^2 + (cos a)^2 = 1 FOR ALL approx a"
      $ quickCheck \aPre ->
          let
            -- given
            (ArbitraryApprox a) = aPre

            -- when
            result = (sinA a) ^ 2 + (cosA a) ^ 2

            -- then
            expected = one

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show a, showA a ]
          in
            result `consistentOp` expected
