module Test.Expression.Evaluator
  ( evaluatorTests
  ) where

import Test.Unit (TestSuite)
import Test.Expression.Evaluator.Evaluate (evaluateTests)

evaluatorTests :: TestSuite
evaluatorTests = do
  evaluateTests