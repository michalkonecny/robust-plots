module Test.IntervalArith.Approx.ShowA ( 
    showATests 
) where

import Prelude

import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

import IntervalArith.Approx (Approx(..), showA, big)

showATests :: TestSuite
showATests =
    suite "IntervalArith.Approx - showA" do
        test "SHOULD show '1.~' WHEN Approx 1 1 0 1" do
            let
                -- given
                input = Approx 1 (big 1) (big 0) 1

                -- when
                result = showA input

                -- then
                expected = "1.~"

            equal expected result