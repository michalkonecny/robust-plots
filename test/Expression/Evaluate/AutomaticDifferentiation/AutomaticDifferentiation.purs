module Test.Expression.Evaluate.AutomaticDifferentiation
  ( automaticDifferentiationTests
  ) where

import Test.Expression.Evaluate.AutomaticDifferentiation.EvaluateDerivative (evaluateDerivativeTests)
import Test.Unit (TestSuite)

automaticDifferentiationTests :: TestSuite
automaticDifferentiationTests = do
  evaluateDerivativeTests
