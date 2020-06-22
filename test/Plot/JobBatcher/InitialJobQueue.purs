module Test.Plot.JobBatcher.InitialJobQueue
  ( initialJobQueueTests
  ) where

import Prelude
import Data.Maybe (Maybe(..), isNothing)
import Data.Set (isEmpty) as S
import Plot.Queue (null) as Q
import Plot.JobBatcher (initialJobQueue)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal, assert)

initialJobQueueTests :: TestSuite
initialJobQueueTests =
  suite "Plot.JobBatcher - initialJobQueue" do
    test "SHOULD have valid initial state" do
      let
        -- given
        jobQueue = initialJobQueue

        -- then
        expectedCurrentId = 0

        expectedRunning = Nothing
      equal expectedCurrentId jobQueue.currentId
      assert "'running' is not Nothing" $ isNothing jobQueue.running
      assert "'cancelled' not empty" $ S.isEmpty jobQueue.cancelled
      assert "'queue' not empty" $ Q.null jobQueue.queue
