module Test.Plotters.Plot1
  ( plot1Tests
  ) where

import Prelude
import Plotters (plot1)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

plot1Tests :: TestSuite
plot1Tests =
  suite "Plotters - plot1" do
    test "ASSERT y = 1.000000000000000000 WHEN x = 0.0" do
      let
        -- given
        x = 0.0

        -- when
        result = plot1 x

        -- then
        expectedY = 1.0

      equal expectedY result
    test "ASSERT y = 0.009900990099009901 WHEN x = 1.0" do
      let
        -- given
        x = 1.0

        -- when
        result = plot1 x

        -- then
        expectedY = 0.009900990099009901

      equal expectedY result
    test "ASSERT y = 0.009900990099009901 WHEN x = -1.0" do
      let
        -- given
        x = -1.0

        -- when
        result = plot1 x

        -- then
        expectedY = 0.009900990099009901

      equal expectedY result
    test "ASSERT y = 0.038461538461538464 WHEN x = 0.5" do
      let
        -- given
        x = 0.5

        -- when
        result = plot1 x

        -- then
        expectedY = 0.038461538461538464

      equal expectedY result
    test "ASSERT y = 0.038461538461538464 WHEN x = -0.5" do
      let
        -- given
        x = -0.5

        -- when
        result = plot1 x

        -- then
        expectedY = 0.038461538461538464

      equal expectedY result
    test "ASSERT y = 0.137931034482758620 WHEN x = 0.25" do
      let
        -- given
        x = 0.25

        -- when
        result = plot1 x

        -- then
        expectedY = 0.137931034482758620

      equal expectedY result
    test "ASSERT y = 0.137931034482758620 WHEN x = -0.25" do
      let
        -- given
        x = -0.25

        -- when
        result = plot1 x

        -- then
        expectedY = 0.137931034482758620

      equal expectedY result
