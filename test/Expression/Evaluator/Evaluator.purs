module Test.Expression.Evaluator
  ( evaluatorTests
  ) where

import Prelude
import Test.Expression.Evaluator.AutomaticDifferentiation.EvaluateDerivative (evaluateDerivativeTests)
import Test.Expression.Evaluator.Evaluate (evaluateTests)
import Test.Expression.Evaluator.RoughEvaluate (roughEvaluateTests)
import Test.Unit (TestSuite)

evaluatorTests :: TestSuite
evaluatorTests = do
  roughEvaluateTests
  evaluateTests
  evaluateDerivativeTests
