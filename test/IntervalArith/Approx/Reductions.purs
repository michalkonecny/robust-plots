module Test.IntervalArith.Approx.Reductions
  ( approxTests_Reductions
  ) where

import Prelude
import IntervalArith.Approx (boundErrorTerm, setMB, (⊑))
import Test.IntervalArith.Approx.Arbitrary (ArbitraryApprox(..))
import Test.IntervalArith.Misc (ArbitraryPositiveExponent(..))
import Test.TestUtils (assertOp)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

approxTests_Reductions :: TestSuite
approxTests_Reductions =
  suite "IntervalArith.Approx - conversions and reductions" do
    test "SHOULD HOLD setMB mb a ⊑ a FOR ALL approx a and integer mb>=0"
      $ quickCheck \aPre mbPre ->
          let
            -- given
            (ArbitraryApprox a) = aPre

            (ArbitraryPositiveExponent mb) = mbPre

            -- when
            aMB = setMB mb a
          -- then
          in
            assertOp (⊑) " ⊑ " aMB a
    test "SHOULD HOLD boundErrorTerms a ⊑ a FOR ALL approx a"
      $ quickCheck \aPre ->
          let
            -- given
            (ArbitraryApprox a) = aPre

            -- when
            aB = boundErrorTerm a
          -- then
          in
            assertOp (⊑) " ⊑ " aB a
