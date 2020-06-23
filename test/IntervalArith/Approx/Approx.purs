module Test.IntervalArith.Approx
  ( approxTests
  ) where

import Prelude
import Test.Field (fieldTests)
import Test.IntervalArith.Approx.Arbitrary (approxEqParams)
import Test.IntervalArith.Approx.FromRational (approxTests_fromRational, approxTests_fromRationalBounds)
import Test.IntervalArith.Approx.Order (approxTests_Consistent, approxTests_Order)
import Test.IntervalArith.Approx.Reductions (approxTests_Reductions)
import Test.IntervalArith.Approx.ShowA (approxTests_showA)
import Test.Unit (TestSuite)

approxTests :: TestSuite
approxTests = do
  approxTests_showA
  approxTests_Reductions
  approxTests_Order
  approxTests_Consistent
  approxTests_Field
  approxTests_fromRational
  approxTests_fromRationalBounds

approxTests_Field :: TestSuite
approxTests_Field = fieldTests approxEqParams
