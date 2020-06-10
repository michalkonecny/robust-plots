module Test.Expression.SubExpression.JoinCommonSubExpressions
  ( joinCommonSubExpressionsTests
  ) where

import Prelude
import Data.Either (Either(..))
import Expression.Error (Expect, throw)
import Expression.Evaluator (presetConstants)
import Expression.Parser (parse)
import Expression.SubExpression (joinCommonSubExpressions)
import Expression.Syntax (Expression)
import Expression.VariableMap (VariableMap)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

joinCommonSubExpressionsTests :: TestSuite
joinCommonSubExpressionsTests =
  suite "Expression.SubExpression - joinCommonSubExpressions" do
    test "ASSERT f(x) = 9.0 WHEN f(x) = sin(sin(sin(x))) + sin(sin(x)) + sin(x)" do
      let
        -- given
        variables = presetConstants

        rawExpression = "sin(sin(sin(x))) + sin(sin(x)) + sin(x)"

        -- when
        result = fromExpect $ parseAndJoinCommonSubExpressions variables rawExpression

        -- then
        expectedResult = show 9.0
      equal expectedResult result

parseAndJoinCommonSubExpressions :: VariableMap Number -> String -> Expect Expression
parseAndJoinCommonSubExpressions variables rawExpression = result
  where
  expressionOrParseError = parse rawExpression

  result = case expressionOrParseError of
    Right expression -> pure $ joinCommonSubExpressions expression
    Left error -> throw error

fromExpect :: Expect Expression -> String
fromExpect (Right expression) = show expression

fromExpect (Left error) = show error
