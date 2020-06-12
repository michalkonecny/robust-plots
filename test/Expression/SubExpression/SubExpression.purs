module Test.Expression.SubExpression
  ( subExpressionTests
  ) where

import Prelude
import Test.Unit (TestSuite)
import Test.Expression.SubExpression.JoinCommonSubExpressions (joinCommonSubExpressionsTests)
import Test.Expression.SubExpression.SplitSubExpressions (splitSubExpressionsTests)
import Test.Expression.SubExpression.IndexToSubExpressionMap (indexToSubExpressionMapTests)
import Test.Expression.SubExpression.SubstituteSubExpressions (substituteSubExpressionsTests)
import Test.Expression.SubExpression.OrderDepencencies (orderDepencenciesTests)

subExpressionTests :: TestSuite
subExpressionTests = do
  splitSubExpressionsTests
  indexToSubExpressionMapTests
  substituteSubExpressionsTests
  orderDepencenciesTests
  joinCommonSubExpressionsTests