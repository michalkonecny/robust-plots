module Test.Plot.GridLines.Step
  ( stepTests
  ) where

import Prelude
import Plot.GridLines (step)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

stepTests :: TestSuite
stepTests =
  suite "Plot.GridLines - step" do
    test "ASSERT setp = 0.1 WHEN range = 2.0" do
      let
        range = 2.0

        expected = 0.1
      equal expected $ step range
