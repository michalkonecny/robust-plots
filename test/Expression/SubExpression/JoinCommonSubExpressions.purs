module Test.Expression.SubExpression.JoinCommonSubExpressions
  ( joinCommonSubExpressionsTests
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Expression.Error (Expect, throw)
import Expression.Evaluator (presetConstants, roughEvaluate)
import Expression.Parser (parse)
import Expression.SubExpression (joinCommonSubExpressions)
import Expression.Syntax (Expression)
import Test.QuickCheck (Result(..), assertEquals)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

joinCommonSubExpressionsTests :: TestSuite
joinCommonSubExpressionsTests =
  suite "Expression.SubExpression - joinCommonSubExpressions" do
    test "ASSERT f(x) = let $v1 = sinx in let $v2 = sin$v1 in let $v3 = sin$v2 in let $v4 = $v3+$v2 in $v4+$v1 WHEN f(x) = sin(sin(sin(x))) + sin(sin(x)) + sin(x)" do
      let
        -- given
        rawExpression = "sin(sin(sin(x))) + sin(sin(x)) + sin(x)"

        -- when
        result = fromExpect $ parseAndJoinCommonSubExpressions rawExpression

        -- then
        expectedResult = "let $v1 = sinx in let $v2 = sin$v1 in let $v3 = sin$v2 in let $v4 = $v3+$v2 in $v4+$v1"
      equal expectedResult result
    test "ASSERT f(x) = let $v1 = sinx in let $v2 = sin$v1 in $v2+$v1 WHEN f(x) = sin(sin(x)) + sin(x)" do
      let
        -- given
        rawExpression = "sin(sin(x)) + sin(x)"

        -- when
        result = fromExpect $ parseAndJoinCommonSubExpressions rawExpression

        -- then
        expectedResult = "let $v1 = sinx in let $v2 = sin$v1 in $v2+$v1"
      equal expectedResult result
    test "ASSERT f(x) = x WHEN f(x) = x" do
      let
        -- given
        rawExpression = "x"

        -- when
        result = fromExpect $ parseAndJoinCommonSubExpressions rawExpression

        -- then
        expectedResult = "x"
      equal expectedResult result
    test "ASSERT f(x) = x+x WHEN f(x) = x+x" do
      let
        -- given
        rawExpression = "x+x"

        -- when
        result = fromExpect $ parseAndJoinCommonSubExpressions rawExpression

        -- then
        expectedResult = "x+x"
      equal expectedResult result
    test "ASSERT f(x) = let $v1 = x+x in $v1+$v1 WHEN f(x) = (x+x)+(x+x)" do
      let
        -- given
        rawExpression = "(x+x)+(x+x)"

        -- when
        result = fromExpect $ parseAndJoinCommonSubExpressions rawExpression

        -- then
        expectedResult = "let $v1 = x+x in $v1+$v1"
      equal expectedResult result
    test "ASSERT f(x) = let $v2 = x+x in let $v4 = $v2+$v2 in let $v1 = sin$v4 in let $v3 = $v1+$v2 in $v3+$v1 WHEN f(x) = sin((x+x)+(x+x))+(x+x)+sin((x+x)+(x+x))" do
      let
        -- given
        rawExpression = "sin((x+x)+(x+x))+(x+x)+sin((x+x)+(x+x))"

        -- when
        result = fromExpect $ parseAndJoinCommonSubExpressions rawExpression

        -- then
        expectedResult = "let $v2 = x+x in let $v4 = $v2+$v2 in let $v1 = sin$v4 in let $v3 = $v1+$v2 in $v3+$v1"
      equal expectedResult result
    test "ASSERT yeilds the same result WHEN f(x) = sin((x+x)+(x+x))+(x+x)+sin((x+x)+(x+x)) WHERE x = n FOR ANY integer n" $ quickCheck
      $ \(n :: Int) -> do
          let
            -- given
            variables = presetConstants <> [ Tuple "x" (toNumber n) ]

            rawExpression = "sin((x+x)+(x+x))+(x+x)+sin((x+x)+(x+x))"
          case parse rawExpression of
            Left error -> Failed $ show error
            Right expression -> case roughEvaluate variables (joinCommonSubExpressions expression), roughEvaluate variables expression of
              Right joinedValue, Right value -> assertEquals joinedValue value
              Right _, Left error -> Failed $ show error
              Left error, Right _ -> Failed $ show error
              Left _, Left _ -> Failed "Failed to evaluate expression"

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
