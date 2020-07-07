module Test.Plot.GridLines
  ( gridLinesTests
  ) where

import Test.Plot.GridLines.CalculateInterval (calculateIntervalTests)
import Test.Unit (TestSuite)

gridLinesTests :: TestSuite
gridLinesTests = do
  calculateIntervalTests
