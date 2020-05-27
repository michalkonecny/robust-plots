module Test.IntervalArith
  ( intervalArithTests
  ) where

import Prelude
import Test.IntervalArith.Approx (approxTests)
import Test.IntervalArith.Dyadic (dyadicTests)
import Test.IntervalArith.Misc (miscTests)
import Test.Unit (TestSuite)

intervalArithTests :: TestSuite
intervalArithTests = do
  miscTests
  dyadicTests
  approxTests
