module Test.Plot.JobBatcher.HasJobs
  ( hasJobsTests
  ) where

import Prelude
import Components.Main.Helper (initialBounds)
import Plot.Commands (PlotCommand(..))
import Plot.JobBatcher (Job, hasJobs, initialJobQueue)
import Misc.Queue (push)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, assertFalse)

hasJobsTests :: TestSuite
hasJobsTests =
  suite "Plot.JobBatcher - hasJobs" do
    test "SHOULD return false WHEN queue is empty" do
      let
        jobQueue = initialJobQueue
      assertFalse "should be false when queue is empty" $ hasJobs jobQueue
    test "SHOULD return true WHEN queue has one job" do
      let
        emptyQueue = initialJobQueue

        jobQueue = emptyQueue { queue = push emptyQueue.queue basicJob }
      assert "should be true when queue is not empty" $ hasJobs jobQueue

basicJob :: Job
basicJob =
  { id: 0
  , command: Empty initialBounds
  , batchId: 0
  }
