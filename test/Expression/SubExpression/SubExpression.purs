module Test.Expression.SubExpression
  ( subExpressionTests
  ) where

import Test.Unit (TestSuite)
import Test.Expression.SubExpression.JoinCommonSubExpressions (joinCommonSubExpressionsTests)

subExpressionTests :: TestSuite
subExpressionTests = do
  joinCommonSubExpressionsTests