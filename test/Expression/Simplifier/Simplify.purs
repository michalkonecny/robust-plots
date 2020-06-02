module Test.Expression.Simplifier.Simplify
  ( simplifyTests
  ) where

import Prelude

import Data.Either (Either(..))
import Expression.Error (Expect, throw)
import Expression.Parser (parse)
import Expression.Simplifier (simplify)
import Expression.Syntax (Expression)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

simplifyTests :: TestSuite
simplifyTests =
  suite "Expression.Simplifier - simplify" do
    test "ASSERT simplified f(x) = 1 WHEN f(x) = 1" do
      let
        -- given
        rawExpression = "1"

        -- when
        result = fromExpect $ parseAndSimplify rawExpression

        -- then
        expectedResult = "1"
      equal expectedResult result

parseAndSimplify :: String -> Expect Expression
parseAndSimplify rawExpression = valueOrEvaluationError
  where
  expressionOrParseError = parse rawExpression

  valueOrEvaluationError = case expressionOrParseError of
    Right expression -> pure $ simplify expression
    Left error -> throw error

fromExpect :: Expect Expression -> String
fromExpect (Right value) = show value

fromExpect (Left error) = show error
