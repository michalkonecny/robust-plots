module Test.Expression.Evaluator
  ( evaluatorTests
  ) where

import Test.Unit (TestSuite)
import Test.Expression.Evaluator.RoughEvaluate (roughEvaluateTests)

evaluatorTests :: TestSuite
evaluatorTests = do
  roughEvaluateTests