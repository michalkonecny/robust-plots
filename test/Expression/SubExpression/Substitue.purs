module Test.Expression.SubExpression.Substitute
  ( substituteTests
  ) where

import Prelude
import Data.Either (Either(..))
import Expression.Error (Expect, multipleErrors, throw)
import Expression.Parser (parse)
import Expression.SubExpression (substitute)
import Expression.Syntax (Expression)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

substituteTests :: TestSuite
substituteTests =
  suite "Expression.SubExpression - substitute" do
    test "ASSERT f(x) = ((sin(sin$v1))+(sin$v1))+$v1 WHEN f(x) = sin(sin(sin(x)))+sin(sin(x))+sin(x) AND $v1 = sin(x)" do
      let
        -- given
        rawExpression = "sin(sin(sin(x))) + sin(sin(x)) + sin(x)"

        rawSubstitueExpression = "sin(x)"

        name = "$v1"

        -- when
        result = fromExpect $ parseAndSubstitute name rawExpression rawSubstitueExpression

        -- then
        expectedResult = "((sin(sin$v1))+(sin$v1))+$v1"
      equal expectedResult result
    test "ASSERT f(x) = $v1+$v1 WHEN f(x) = sin(x)+sin(x) AND $v1 = sin(x)" do
      let
        -- given
        rawExpression = "sin(x) + sin(x)"

        rawSubstitueExpression = "sin(x)"

        name = "$v1"

        -- when
        result = fromExpect $ parseAndSubstitute name rawExpression rawSubstitueExpression

        -- then
        expectedResult = "$v1+$v1"
      equal expectedResult result
    test "ASSERT f(x) = $v1^$v1 WHEN f(x) = (2*x)^(2*x) AND $v1 = 2*x" do
      let
        -- given
        rawExpression = "(2*x)^(2*x)"

        rawSubstitueExpression = "2*x"

        name = "$v1"

        -- when
        result = fromExpect $ parseAndSubstitute name rawExpression rawSubstitueExpression

        -- then
        expectedResult = "$v1^$v1"
      equal expectedResult result

parseAndSubstitute :: String -> String -> String -> Expect Expression
parseAndSubstitute name rawExpression rawSubstitueExpression = case parse rawExpression, parse rawSubstitueExpression of
  Right expression, Right substitueExpression -> pure $ substitute { name, expression: substitueExpression } expression
  Left e1, Left e2 -> multipleErrors $ (show e1) <> " | " <> (show e2)
  Left e1, _ -> throw e1
  _, Left e2 -> throw e2

fromExpect :: Expect Expression -> String
fromExpect (Right expression) = show expression

fromExpect (Left error) = show error
