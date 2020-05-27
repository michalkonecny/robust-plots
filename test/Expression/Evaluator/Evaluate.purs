module Test.Expression.Evaluator.Evaluate
  ( evaluateTests
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Expression.Error (Expect)
import Expression.Evaluator (VariableMap, evaluate)
import Expression.Parser (parse)
import Expression.Syntax (Expression)
import Math (e, pi)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

evaluateTests :: TestSuite
evaluateTests =
  suite "Expression.Evaluator - evaluate" do
    test "ASSERT f(x) = 1 WHEN f(x) = 1" do
      let
        -- given
        variables = presetConstants

        rawExpression = "1"

        -- when
        result = toStr $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 1
      equal expectedResult result
    test "ASSERT f(x) = 5 WHEN f(x) = 5" do
      let
        -- given
        variables = presetConstants

        rawExpression = "5"

        -- when
        result = toStr $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 5
      equal expectedResult result
    test "ASSERT f(x) = 4 WHEN f(x) = 2.0*x AND x = 2.0" do
      let
        -- given
        x = 2.0

        variables = [ (Tuple "x" x) ] <> presetConstants

        rawExpression = "2.0*x"

        -- when
        result = toStr $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 4
      equal expectedResult result
    test "ASSERT f(x) = 4 WHEN f(x) = 2*x AND x = 2.0" do
      let
        -- given
        x = 2.0

        variables = [ (Tuple "x" x) ] <> presetConstants

        rawExpression = "2.0*x"

        -- when
        result = toStr $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 4
      equal expectedResult result

parseAndEvaluate :: VariableMap Number -> String -> Expect Expression
parseAndEvaluate variables rawExpression = result
  where
    expressionOrParseError = parse rawExpression

    expressionOrEvaluationError = case expressionOrParseError of
      Right expression -> evaluate variables expression
      Left error -> expressionOrParseError

    result = expressionOrEvaluationError

toStr :: Expect Expression -> String 
toStr (Right expression) = show expression
toStr (Left error) = show error

presetConstants :: Array (Tuple String Number)
presetConstants = [ (Tuple "pi" pi), (Tuple "e" e) ]