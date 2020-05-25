module Test.Plotters
  ( plottersTests
  ) where

import Test.Unit (TestSuite)
import Test.Plotters.Plot1 (plot1Tests)

plottersTests :: TestSuite
plottersTests = do
  plot1Tests