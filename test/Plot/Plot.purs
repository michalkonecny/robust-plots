module Test.Plot
  ( plotTests
  ) where

import Test.Expression.JobBatcher (jobBatcherTests)
import Test.Unit (TestSuite)

plotTests :: TestSuite
plotTests = do
  jobBatcherTests