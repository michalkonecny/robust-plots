module Test.IntervalArith
  ( intervalArithTests
  ) where

import Prelude
import Test.Unit (TestSuite)
import Test.IntervalArith.Approx (approxTests)
import Test.IntervalArith.Misc (miscTests)

intervalArithTests :: TestSuite
intervalArithTests = do
  miscTests
  approxTests
