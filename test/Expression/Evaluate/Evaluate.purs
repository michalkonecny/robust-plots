module Test.Expression.Evaluate
  ( evaluateTests
  ) where

import Test.Expression.Evaluate.AutomaticDifferentiation (automaticDifferentiationTests)
import Test.Unit (TestSuite)

evaluateTests :: TestSuite
evaluateTests = do
  automaticDifferentiationTests
