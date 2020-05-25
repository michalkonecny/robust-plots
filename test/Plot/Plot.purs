module Test.Plot
  ( plotTests
  ) where

import Test.Unit (TestSuite)
import Test.Plot.Functions (functionsTests)

plotTests :: TestSuite
plotTests = do
  functionsTests