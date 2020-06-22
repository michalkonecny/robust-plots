module Test.Plot.JobBatcher
  ( jobBatcherTests
  ) where

import Prelude
import Test.Unit (TestSuite)
import Test.Plot.JobBatcher.InitialJobQueue (initialJobQueueTests)
import Test.Plot.JobBatcher.HasJobs (hasJobsTests)
import Test.Plot.JobBatcher.ClearCancelled (clearCancelledTests)
import Test.Plot.JobBatcher.CancelAll (cancelAllTests)
import Test.Plot.JobBatcher.CancelWithBatchId (cancelWithBatchIdTests)
import Test.Plot.JobBatcher.SetRunning (setRunningTests)
import Test.Plot.JobBatcher.IsCancelled (isCancelledTests)

jobBatcherTests :: TestSuite
jobBatcherTests = do
  initialJobQueueTests
  hasJobsTests
  clearCancelledTests
  cancelAllTests
  cancelWithBatchIdTests
  setRunningTests
  isCancelledTests
