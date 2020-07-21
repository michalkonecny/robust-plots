module Test.Expression
  ( expressionTests
  ) where

import Prelude
import Test.Expression.Evaluator (evaluatorTests)
import Test.Expression.Parser (parserTests)
import Test.Expression.Simplifier (simplifierTests)
import Test.Expression.VariableMap (variableMapTests)
import Test.Expression.SubExpression (subExpressionTests)
import Test.Expression.Evaluate (evaluateTests)
import Test.Unit (TestSuite)

expressionTests :: TestSuite
expressionTests = do
  parserTests
  evaluatorTests
  simplifierTests
  variableMapTests
  subExpressionTests
  evaluateTests