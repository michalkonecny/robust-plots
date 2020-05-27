module Test.Expression
  ( expressionTests
  ) where

import Test.Expression.Evaluator (evaluatorTests)
import Test.Unit (TestSuite)

expressionTests :: TestSuite
expressionTests = do
  evaluatorTests