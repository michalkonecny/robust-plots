module Test.Plot.JobBatcher.ClearCancelled
  ( clearCancelledTests
  ) where

import Prelude
import Data.Set (insert, isEmpty)
import Plot.JobBatcher (clearCancelled, initialJobQueue)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, assertFalse)

clearCancelledTests :: TestSuite
clearCancelledTests =
  suite "Plot.JobBatcher - clearCancelled" do
    test "SHOULD reset to empty set" do
      let
        jobQueue = initialJobQueue

        jobQueueWithCancelled = jobQueue { cancelled = insert 3 jobQueue.cancelled }
      assertFalse "should not be empty when has cancelled jobs" $ isEmpty jobQueueWithCancelled.cancelled
      let
        jobQueueWithClearedCancelled = clearCancelled jobQueueWithCancelled
      assert "should be empty when has no cancelled jobs" $ isEmpty jobQueueWithClearedCancelled.cancelled
