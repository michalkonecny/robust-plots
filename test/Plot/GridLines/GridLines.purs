module Test.Plot.GridLines
  ( gridLinesTests
  ) where

import Test.Plot.GridLines.Step (stepTests)
import Test.Unit (TestSuite)

gridLinesTests :: TestSuite
gridLinesTests = do
  stepTests
