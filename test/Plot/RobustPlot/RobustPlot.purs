module Test.Plot.RobustPlot
  ( robustPlotTests
  ) where

import Test.Unit (TestSuite)
import Test.Plot.RobustPlot.SegmentDomain (segmentDomainTests)

robustPlotTests :: TestSuite
robustPlotTests = do
  segmentDomainTests
