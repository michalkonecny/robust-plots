module Test.Expression.JobBatcher.CancelWithBatchId
  ( cancelWithBatchIdTests
  ) where

import Prelude
import Components.Main.Helper (initialBounds)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..), isNothing)
import Data.Set (fromFoldable, isEmpty)
import Plot.Commands (PlotCommand(..))
import Plot.JobBatcher (Job, hasJobs, initialJobQueue, cancelWithBatchId)
import Plot.Queue (Queue, push)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, assertFalse, equal)
import Types (Id)

cancelWithBatchIdTests :: TestSuite
cancelWithBatchIdTests =
  suite "Plot.JobBatcher - cancelWithBatchId" do
    test "SHOULD cancel jobs with batch id WHEN queue has 3 jobs AND no job running AND batch id = 1" do
      let
        emptyQueue = initialJobQueue

        batchId = 1

        jobs = insertAll [ basicJob 1 batchId, basicJob 2 batchId, basicJob 3 batchId ] emptyQueue.queue

        jobQueue = emptyQueue { queue = jobs }
      assert "should be true when queue is not empty" $ hasJobs jobQueue
      let
        cancelledQueue = cancelWithBatchId jobQueue batchId

        expectedJobIds = fromFoldable [ 1, 2, 3 ]
      equal expectedJobIds cancelledQueue.cancelled
      assertFalse "should be false when all jobs are cancelled" $ hasJobs cancelledQueue
    test "SHOULD cancel all jobs WHEN queue has 3 jobs AND a job running AND batch id = 1" do
      let
        emptyQueue = initialJobQueue

        batchId = 1

        jobs = insertAll [ basicJob 1 batchId, basicJob 2 batchId, basicJob 3 batchId ] emptyQueue.queue

        running = basicJob 4 batchId

        jobQueue = emptyQueue { queue = jobs, running = Just running }
      assert "should be true when queue is not empty" $ hasJobs jobQueue
      let
        cancelledQueue = cancelWithBatchId jobQueue batchId

        expectedJobIds = fromFoldable [ 1, 2, 3, 4 ]
      equal expectedJobIds cancelledQueue.cancelled
      assertFalse "should be false when all jobs are cancelled" $ hasJobs cancelledQueue
      assert "should be 'Nothing' when running has been cancelled" $ isNothing cancelledQueue.running
    test "SHOULD cancel no jobs WHEN queue has 3 jobs AND no job running AND given batch id has no jobs" do
      let
        emptyQueue = initialJobQueue

        batchId = 1

        jobs = insertAll [ basicJob 1 batchId, basicJob 2 batchId, basicJob 3 batchId ] emptyQueue.queue

        jobQueue = emptyQueue { queue = jobs }
      assert "should be true when queue is not empty" $ hasJobs jobQueue
      let
        cancelledQueue = cancelWithBatchId jobQueue 2 -- Different batch id
      assert "should be empty when no jobs were cancelled" $ isEmpty cancelledQueue.cancelled
    test "SHOULD cancel no jobs WHEN queue has 3 jobs AND a job running AND given batch id has no jobs" do
      let
        emptyQueue = initialJobQueue

        batchId = 1

        jobs = insertAll [ basicJob 1 batchId, basicJob 2 batchId, basicJob 3 batchId ] emptyQueue.queue

        running = basicJob 4 batchId

        jobQueue = emptyQueue { queue = jobs, running = Just running }
      assert "should be true when queue is not empty" $ hasJobs jobQueue
      let
        cancelledQueue = cancelWithBatchId jobQueue 2
      assert "should be empty when no jobs were cancelled" $ isEmpty cancelledQueue.cancelled

basicJob :: Id -> Id -> Job
basicJob id batchId =
  { id
  , command: Empty initialBounds
  , batchId
  }

insertAll :: forall a f. Foldable f => f a -> Queue a -> Queue a
insertAll toInsert set = foldl push set toInsert
