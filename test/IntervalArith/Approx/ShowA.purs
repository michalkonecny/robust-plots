module Test.IntervalArith.Approx.ShowA
  ( showATests
  ) where

import Prelude
import IntervalArith.Approx (Approx(..), showA)
import IntervalArith.Misc (big)
import Test.QuickCheck ((===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

showATests :: TestSuite
showATests =
  suite "IntervalArith.Approx - showA" do
    test "SHOULD show '1' WHEN Approx 1 1 0 0" do
      let
        -- given
        input = Approx 1 (big 1) (big 0) 0

        -- when
        result = showA input

        -- then
        expected = "1"
      equal expected result
    test "SHOULD format (Approx 1 n 0 0) as n FOR ANY integer n"
      $ quickCheck \n ->
          let
            -- given
            input = Approx 1 (big n) (big 0) 0

            -- when
            result = showA input

            -- then
            expected = show n
          in
            expected === result
