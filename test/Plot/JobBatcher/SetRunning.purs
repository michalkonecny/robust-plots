module Test.Expression.JobBatcher.SetRunning
  ( setRunningTests
  ) where

import Prelude
import Components.Main.Helper (initialBounds)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Plot.Commands (PlotCommand(..))
import Plot.JobBatcher (Job, JobQueue, hasJobs, initialJobQueue, setRunning)
import Plot.Queue (Queue, push)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, assertFalse)
import Types (Id)

setRunningTests :: TestSuite
setRunningTests =
  suite "Plot.JobBatcher - setRunning" do
    test "SHOULD set only job in queue as running WHEN queue has 1 job" do
      let
        emptyQueue = initialJobQueue

        jobs = insertAll [ basicJob 1 ] emptyQueue.queue

        jobQueue = emptyQueue { queue = jobs }
      assert "should be true when queue is not empty" $ hasJobs jobQueue
      let
        cancelledQueue = setRunning jobQueue
      assert "should be the first job on the queue" $ isRunning (\job -> job.id == 1) cancelledQueue
      assertFalse "should be false when only job is now running" $ hasJobs cancelledQueue
    test "SHOULD set no job as running WHEN queue has no jobs" do
      let
        emptyQueue = initialJobQueue
      assertFalse "should be true when queue is not empty" $ hasJobs emptyQueue
      let
        cancelledQueue = setRunning emptyQueue
      assertFalse "should be `Nothing` when no jobs were on the queue" $ isRunning (const true) cancelledQueue
      assertFalse "should be false when queue is empty" $ hasJobs cancelledQueue

basicJob :: Id -> Job
basicJob id =
  { id
  , command: Empty initialBounds
  , batchId: 0
  }

isRunning :: (Job -> Boolean) -> JobQueue -> Boolean
isRunning check jobQueue = case jobQueue.running of
  Nothing -> false
  Just job -> check job

insertAll :: forall a f. Foldable f => f a -> Queue a -> Queue a
insertAll toInsert set = foldl push set toInsert
