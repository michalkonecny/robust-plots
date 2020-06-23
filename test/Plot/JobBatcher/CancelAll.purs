module Test.Plot.JobBatcher.CancelAll
  ( cancelAllTests
  ) where

import Prelude
import Components.Main.Helper (initialBounds)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..), isNothing)
import Data.Set (fromFoldable)
import Plot.Commands (PlotCommand(..))
import Plot.JobBatcher (Job, hasJobs, initialJobQueue, cancelAll)
import Plot.Queue (Queue, push)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, assertFalse, equal)
import Types (Id)

cancelAllTests :: TestSuite
cancelAllTests =
  suite "Plot.JobBatcher - cancelAll" do
    test "SHOULD cancel all jobs WHEN queue has 3 jobs AND no job running" do
      let
        emptyQueue = initialJobQueue

        jobs = insertAll [ basicJob 1, basicJob 2, basicJob 3 ] emptyQueue.queue

        jobQueue = emptyQueue { queue = jobs }
      assert "should be true when queue is not empty" $ hasJobs jobQueue
      let
        cancelledQueue = cancelAll jobQueue

        expectedJobIds = fromFoldable [ 1, 2, 3 ]
      equal expectedJobIds cancelledQueue.cancelled
      assertFalse "should be false when all jobs are cancelled" $ hasJobs cancelledQueue
    test "SHOULD cancel all jobs WHEN queue has 3 jobs AND a job running" do
      let
        emptyQueue = initialJobQueue

        jobs = insertAll [ basicJob 1, basicJob 2, basicJob 3 ] emptyQueue.queue

        running = basicJob 4

        jobQueue = emptyQueue { queue = jobs, running = Just running }
      assert "should be true when queue is not empty" $ hasJobs jobQueue
      let
        cancelledQueue = cancelAll jobQueue

        expectedJobIds = fromFoldable [ 1, 2, 3, 4 ]
      equal expectedJobIds cancelledQueue.cancelled
      assertFalse "should be false when all jobs are cancelled" $ hasJobs cancelledQueue
      assert "should be 'Nothing' when running has been cancelled" $ isNothing cancelledQueue.running

basicJob :: Id -> Job
basicJob id =
  { id
  , command: Empty initialBounds
  , batchId: 0
  }

insertAll :: forall a f. Foldable f => f a -> Queue a -> Queue a
insertAll toInsert set = foldl push set toInsert
