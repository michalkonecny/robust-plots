module Test.Expression.Evaluator
  ( evaluatorTests
  ) where

import Prelude
import Test.Unit (TestSuite)
import Test.Expression.Evaluator.RoughEvaluate (roughEvaluateTests)
import Test.Expression.Evaluator.Evaluate (evaluateTests)

evaluatorTests :: TestSuite
evaluatorTests = do
  roughEvaluateTests
  evaluateTests
