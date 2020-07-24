module Test.Expression.Evaluate.AutomaticDifferentiation
  ( automaticDifferentiationTests
  ) where

import Prelude
import Test.Expression.Evaluate.AutomaticDifferentiation.EvaluateDerivative (evaluateDerivativeTests)
import Test.Expression.Evaluate.AutomaticDifferentiation.EvaluateDerivative2 (evaluateDerivative2Tests)
import Test.Unit (TestSuite)

automaticDifferentiationTests :: TestSuite
automaticDifferentiationTests = do
  evaluateDerivativeTests
  evaluateDerivative2Tests
