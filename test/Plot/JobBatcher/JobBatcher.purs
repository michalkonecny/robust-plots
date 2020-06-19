module Test.Expression.JobBatcher
  ( jobBatcherTests
  ) where

import Prelude
import Test.Unit (TestSuite)
import Test.Expression.JobBatcher.InitialJobQueue (initialJobQueueTests)
import Test.Expression.JobBatcher.HasJobs (hasJobsTests)

jobBatcherTests :: TestSuite
jobBatcherTests = do
  initialJobQueueTests
  hasJobsTests
