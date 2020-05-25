module Test.Plot.Functions
  ( functionsTests
  ) where

import Test.Unit (TestSuite)
import Test.Plot.Functions.Plot1 (plot1Tests)

functionsTests :: TestSuite
functionsTests = do
  plot1Tests