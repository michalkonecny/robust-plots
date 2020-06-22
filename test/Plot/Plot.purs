module Test.Plot
  ( plotTests
  ) where

import Test.Plot.JobBatcher (jobBatcherTests)
import Test.Unit (TestSuite)

plotTests :: TestSuite
plotTests = do
  jobBatcherTests