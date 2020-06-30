module Test.Plot
  ( plotTests
  ) where

import Prelude
import Test.Plot.JobBatcher (jobBatcherTests)
import Test.Plot.RobustPlot (robustPlotTests)
import Test.Unit (TestSuite)

plotTests :: TestSuite
plotTests = do
  jobBatcherTests
  robustPlotTests