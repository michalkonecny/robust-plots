module Test.IntervalArith.Approx
  ( approxTests
  ) where

import Prelude

import Test.Field (fieldTests)
import Test.IntervalArith.Approx.ApproxOrder (approxTests_Consistent, approxTests_ApproxOrder)
import Test.IntervalArith.Approx.Arbitrary (approxEqParams)
import Test.IntervalArith.Approx.FromRational (approxTests_fromRational, approxTests_fromRationalBounds)
import Test.IntervalArith.Approx.NumOrder (approxTests_NumOrder)
import Test.IntervalArith.Approx.Reductions (approxTests_Reductions)
import Test.IntervalArith.Approx.ShowA (approxTests_showA)
import Test.Unit (TestSuite)

approxTests :: TestSuite
approxTests = do
  approxTests_showA
  approxTests_Reductions
  approxTests_fromRational
  approxTests_fromRationalBounds
  approxTests_ApproxOrder
  approxTests_Consistent
  approxTests_NumOrder
  -- approxTests_absA
  approxTests_Field
  -- approxTests_SqrtA

approxTests_Field :: TestSuite
approxTests_Field = fieldTests approxEqParams
