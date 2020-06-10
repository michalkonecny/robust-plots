module Test.Expression.SubExpression.JoinCommonSubExpressions
  ( joinCommonSubExpressionsTests
  ) where

import Prelude
import Data.Either (Either(..))
import Expression.Error (Expect, throw)
import Expression.Parser (parse)
import Expression.SubExpression (joinCommonSubExpressions)
import Expression.Syntax (Expression)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

joinCommonSubExpressionsTests :: TestSuite
joinCommonSubExpressionsTests =
  suite "Expression.SubExpression - joinCommonSubExpressions" do
    test "ASSERT f(x) = let $v2 = sin$v1 in let $v1 = sinx in ((sin$v2)+$v2)+$v1 WHEN f(x) = sin(sin(sin(x))) + sin(sin(x)) + sin(x)" do
      let
        -- given
        rawExpression = "sin(sin(sin(x))) + sin(sin(x)) + sin(x)"

        -- when
        result = fromExpect $ parseAndJoinCommonSubExpressions rawExpression

        -- then
        expectedResult = "let $v2 = sin$v1 in let $v1 = sinx in ((sin$v2)+$v2)+$v1"
      equal expectedResult result
    test "ASSERT f(x) = let $v1 = sinx in (sin$v1)+$v1 WHEN f(x) = sin(sin(x)) + sin(x)" do
      let
        -- given
        rawExpression = "sin(sin(x)) + sin(x)"

        -- when
        result = fromExpect $ parseAndJoinCommonSubExpressions rawExpression

        -- then
        expectedResult = "let $v1 = sinx in (sin$v1)+$v1"
      equal expectedResult result

parseAndJoinCommonSubExpressions :: String -> Expect Expression
parseAndJoinCommonSubExpressions rawExpression = result
  where
  expressionOrParseError = parse rawExpression

  result = case expressionOrParseError of
    Right expression -> pure $ joinCommonSubExpressions expression
    Left error -> throw error

fromExpect :: Expect Expression -> String
fromExpect (Right expression) = show expression

fromExpect (Left error) = show error
