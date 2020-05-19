module Test.IntervalArith ( 
    intervalArithTests 
) where

import Test.Unit (TestSuite)

import Test.IntervalArith.Approx (approxTests)

intervalArithTests :: TestSuite
intervalArithTests = do
    approxTests