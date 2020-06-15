module Test.Expression.SubExpression
  ( subExpressionTests
  ) where

import Prelude
import Test.Unit (TestSuite)
import Test.Expression.SubExpression.JoinCommonSubExpressions (joinCommonSubExpressionsTests)
import Test.Expression.SubExpression.SplitSubExpressions (splitSubExpressionsTests)
import Test.Expression.SubExpression.SubExpressionToVariableMap (subExpressionToVariableMapTests)
import Test.Expression.SubExpression.SubstituteSubExpressions (substituteSubExpressionsTests)
import Test.Expression.SubExpression.OrderDepencencies (orderDepencenciesTests)
import Test.Expression.SubExpression.RemoveSubExpressions (removeSubExpressionsTests)

subExpressionTests :: TestSuite
subExpressionTests = do
  splitSubExpressionsTests
  subExpressionToVariableMapTests
  substituteSubExpressionsTests
  orderDepencenciesTests
  joinCommonSubExpressionsTests
  removeSubExpressionsTests
  