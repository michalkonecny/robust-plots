module Test.Expression.JobBatcher.IsCancelled
  ( isCancelledTests
  ) where

import Prelude
import Data.Set (fromFoldable)
import Plot.JobBatcher (initialJobQueue, isCancelled)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, assertFalse)

isCancelledTests :: TestSuite
isCancelledTests =
  suite "Plot.JobBatcher - isCancelled" do
    test "SHOULD be true WHEN id is in cancelled set" do
      let
        jobQueue = initialJobQueue { cancelled = fromFoldable [ 1, 2, 3 ] }
      assert "should be false when all jobs are cancelled" $ isCancelled jobQueue 1
    test "SHOULD be false WHEN id is not in cancelled set" do
      let
        jobQueue = initialJobQueue { cancelled = fromFoldable [ 1, 2, 3 ] }
      assertFalse "should be false when all jobs are cancelled" $ isCancelled jobQueue 4
