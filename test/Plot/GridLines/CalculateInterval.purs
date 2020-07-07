module Test.Plot.GridLines.CalculateInterval
  ( calculateIntervalTests
  ) where

import Prelude
import Plot.GridLines (calculateInterval)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

calculateIntervalTests :: TestSuite
calculateIntervalTests =
  suite "Plot.GridLines - calculateInterval" do
    test "ASSERT interval = 0.1 WHEN range = 2.0" do
      let
        range = 2.0

        expected = 0.1
      equal expected $ calculateInterval range
