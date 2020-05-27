module Test.Expression
  ( expressionTests
  ) where

import Prelude
import Test.Expression.Evaluator (evaluatorTests)
import Test.Expression.Parser (parserTests)
import Test.Unit (TestSuite)

expressionTests :: TestSuite
expressionTests = do
  parserTests
  evaluatorTests