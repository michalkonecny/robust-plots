module Test.IntervalArith.Approx
  ( approxTests
  ) where

import Test.Unit (TestSuite)
import Test.IntervalArith.Approx.ShowA (showATests)

approxTests :: TestSuite
approxTests = do
  showATests
