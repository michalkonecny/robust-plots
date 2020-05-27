module Test.Expression.Evaluator.Evaluate
  ( evaluateTests
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Expression.Error (Expect, throw)
import Expression.Evaluator (VariableMap, evaluate, presetConstants)
import Expression.Parser (parse)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

evaluateTests :: TestSuite
evaluateTests =
  suite "Expression.Evaluator - evaluate" do
    test "ASSERT f(x) = 1.0 WHEN f(x) = 1" do
      let
        -- given
        variables = presetConstants

        rawExpression = "1"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 1.0
      equal expectedResult result
    test "ASSERT f(x) = 5.0 WHEN f(x) = 5" do
      let
        -- given
        variables = presetConstants

        rawExpression = "5"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 5.0
      equal expectedResult result
    test "ASSERT f(x) = 9.0 WHEN f(x) = 4+5" do
      let
        -- given
        variables = presetConstants

        rawExpression = "4+5"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 9.0
      equal expectedResult result
    test "ASSERT f(x) = 7.5 WHEN f(x) = 4.5+3" do
      let
        -- given
        variables = presetConstants

        rawExpression = "4.5+3"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 7.5
      equal expectedResult result
    test "ASSERT f(x) = 12.0 WHEN f(x) = 4*3" do
      let
        -- given
        variables = presetConstants

        rawExpression = "4*3"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 12.0
      equal expectedResult result
    test "ASSERT f(x) = 9.0 WHEN f(x) = 4.5*2" do
      let
        -- given
        variables = presetConstants

        rawExpression = "4.5*2"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 9.0
      equal expectedResult result
    test "ASSERT f(x) = 4.0 WHEN f(x) = 8/2" do
      let
        -- given
        variables = presetConstants

        rawExpression = "8/2"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 4.0
      equal expectedResult result
    test "ASSERT f(x) = 4.5 WHEN f(x) = 9/2" do
      let
        -- given
        variables = presetConstants

        rawExpression = "9/2"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 4.5
      equal expectedResult result
    test "ASSERT f(x) = 4.75 WHEN f(x) = 9.5/2" do
      let
        -- given
        variables = presetConstants

        rawExpression = "9.5/2"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 4.75
      equal expectedResult result
    test "ASSERT f(x) = 4.75 WHEN f(x) = 9.5/2" do
      let
        -- given
        variables = presetConstants

        rawExpression = "9.5/2"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 4.75
      equal expectedResult result
    test "ASSERT f(x) = 7.0 WHEN f(x) = 9-2" do
      let
        -- given
        variables = presetConstants

        rawExpression = "9-2"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 7.0
      equal expectedResult result
    test "ASSERT f(x) = 6.0 WHEN f(x) = 9.5-3.5" do
      let
        -- given
        variables = presetConstants

        rawExpression = "9.5-3.5"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 6.0
      equal expectedResult result
    test "ASSERT f(x) = 12.0 WHEN f(x) = (2+4)+6" do
      let
        -- given
        variables = presetConstants

        rawExpression = "(2+4)+6"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 12.0
      equal expectedResult result
    test "ASSERT f(x) = 4.0 WHEN f(x) = 2.0*x AND x = 2.0" do
      let
        -- given
        x = 2.0

        variables = [ (Tuple "x" x) ] <> presetConstants

        rawExpression = "2.0*x"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 4.0
      equal expectedResult result
    test "ASSERT f(x) = 4.0 WHEN f(x) = 2*x AND x = 2.0" do
      let
        -- given
        x = 2.0

        variables = [ (Tuple "x" x) ] <> presetConstants

        rawExpression = "2.0*x"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 4.0
      equal expectedResult result

parseAndEvaluate :: VariableMap Number -> String -> Expect Number
parseAndEvaluate variables rawExpression = result
  where
  expressionOrParseError = parse rawExpression

  valueOrEvaluationError = case expressionOrParseError of
    Right expression -> evaluate variables expression
    Left error -> throw error

  result = valueOrEvaluationError

fromExpect :: Expect Number -> String
fromExpect (Right value) = show value

fromExpect (Left error) = show error
