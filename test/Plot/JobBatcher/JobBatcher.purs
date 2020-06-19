module Test.Expression.JobBatcher
  ( jobBatcherTests
  ) where

import Prelude
import Test.Unit (TestSuite)
import Test.Expression.JobBatcher.InitialJobQueue (initialJobQueueTests)
import Test.Expression.JobBatcher.HasJobs (hasJobsTests)
import Test.Expression.JobBatcher.ClearCancelled (clearCancelledTests)
import Test.Expression.JobBatcher.CancelAll (cancelAllTests)
import Test.Expression.JobBatcher.CancelWithBatchId (cancelWithBatchIdTests)
import Test.Expression.JobBatcher.SetRunning (setRunningTests)
import Test.Expression.JobBatcher.IsCancelled (isCancelledTests)

jobBatcherTests :: TestSuite
jobBatcherTests = do
  initialJobQueueTests
  hasJobsTests
  clearCancelledTests
  cancelAllTests
  cancelWithBatchIdTests
  setRunningTests
  isCancelledTests
