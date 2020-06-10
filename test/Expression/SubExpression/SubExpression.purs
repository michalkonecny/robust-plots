module Test.Expression.SubExpression
  ( subExpressionTests
  ) where

import Prelude
import Test.Unit (TestSuite)
import Test.Expression.SubExpression.JoinCommonSubExpressions (joinCommonSubExpressionsTests)
import Test.Expression.SubExpression.Substitute (substituteTests)
import Test.Expression.SubExpression.CountOccurances (countOccurancesTests)

subExpressionTests :: TestSuite
subExpressionTests = do
  joinCommonSubExpressionsTests
  substituteTests
  countOccurancesTests