module Test.IntervalArith.Approx.Order
  ( approxTests_Order, approxTests_Consistent
  ) where

import Prelude

import IntervalArith.Approx (Approx(..), consistent)
import IntervalArith.Misc (big)
import Test.IntervalArith.Approx.Arbitrary (ArbitraryApprox(..), approxOrdParams)
import Test.Order (preOrderTests)
import Test.TestUtils (assertOp)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

approxTests_Order :: TestSuite
approxTests_Order =
  suite "IntervalArith.Approx - approximation order (`better`)" do
    test "SHOULD give (Approx 4 12 1 3 = 96±8 \"1~~\") WHEN setMB 4 (Approx 10 100 0 0 ~ \"100\")" do
      let
        -- given
        input = Approx 10 (big 100) (big 0) 0

        -- when
        result = Approx 4 (big 12) (big 1) 3

        -- then
        expected = result
      equal expected result
    preOrderTests approxOrdParams

approxTests_Consistent :: TestSuite
approxTests_Consistent =
  suite "IntervalArith.Approx - consistency check (`consistent`)" do
    test "SHOULD HOLD consistent (Approx 4 12 1 3 = 96±8) (Approx 4 13 1 3 = 104±8)" do
      let
        -- given
        input1 = Approx 4 (big 12) (big 1) 3

        input2 = Approx 4 (big 12) (big 1) 3

        -- when
        result = consistent input1 input2
      -- then
      equal result true
    test "SHOULD HOLD consistent a b WHEN a ⊑ b FOR ALL approx a b"
      $ quickCheck \aPre bPre ->
          let
            -- given
            (ArbitraryApprox b) = bPre

            (ArbitraryApprox a1) = aPre

            -- when
            a = approxOrdParams.makeLeq b a1
          -- then
          in
            assertOp consistent " `consistent` " a b

