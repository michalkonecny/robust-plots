module Test.Expression.Evaluator
  ( evaluatorTests
  ) where

import Prelude
import Test.Unit (TestSuite)
import Test.Expression.Evaluator.Evaluate (evaluateTests)
import Test.Expression.Evaluator.Lookup (lookUpTests)

evaluatorTests :: TestSuite
evaluatorTests = do
  evaluateTests
  lookUpTests