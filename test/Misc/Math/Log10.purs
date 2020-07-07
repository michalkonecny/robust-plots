module Test.Misc.Math.Log10
  ( log10Tests
  ) where

import Prelude
import Misc.Math (log10)
import Test.TestUtils (equalTolerance)
import Test.Unit (TestSuite, suite, test)

log10Tests :: TestSuite
log10Tests =
  suite "Misc.Math - log10" do
    test "ASSERT result = 2.0 WHEN input = 100.0" do
      let
        value = 100.0

        expected = 2.0
      equalTolerance expected $ log10 value
    test "ASSERT result = -1.0 WHEN input = 0.1" do
      let
        value = 0.1

        expected = -1.0
      equalTolerance expected $ log10 value
    test "ASSERT result = 1.0 WHEN input = 10.0" do
      let
        value = 10.0

        expected = 1.0
      equalTolerance expected $ log10 value
    test "ASSERT result = 0.0 WHEN input = 1.0" do
      let
        value = 1.0

        expected = 0.0
      equalTolerance expected $ log10 value
