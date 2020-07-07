module Test.Plot
  ( plotTests
  ) where

import Prelude
import Test.Plot.JobBatcher (jobBatcherTests)
import Test.Plot.Segments (segmentsTests)
import Test.Plot.GridLines (gridLinesTests)
import Test.Unit (TestSuite)

plotTests :: TestSuite
plotTests = do
  jobBatcherTests
  segmentsTests
  gridLinesTests