module Test.Expression.Evaluator.Evaluate
  ( evaluateTests
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Ratio ((%))
import Data.Tuple (Tuple(..))
import Expression.Error (Expect, throw)
import Expression.Evaluator (evaluate)
import Expression.Parser (parse)
import Expression.VariableMap (VariableMap)
import IntervalArith.Approx (Approx, consistent, fromRationalPrec)
import IntervalArith.Misc (big)
import Test.Expression.Helper (expectValue)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

evaluateTests :: TestSuite
evaluateTests =
  suite "Expression.Evaluator - evaluate" do
    test "ASSERT f(x) = 9.0 WHEN f(x) = 4+5" do
      let
        -- given
        rawExpression = "4+5"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 9 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 7.5 WHEN f(x) = 4.5+3" do
      let
        -- given
        rawExpression = "4.5+3"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 15 2
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 12.0 WHEN f(x) = 4*3" do
      let
        -- given
        rawExpression = "4*3"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 12 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 9.0 WHEN f(x) = 4.5*2" do
      let
        -- given
        rawExpression = "4.5*2"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 9 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 4.0 WHEN f(x) = 8/2" do
      let
        -- given
        rawExpression = "8/2"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 4 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 4.5 WHEN f(x) = 9/2" do
      let
        -- given
        rawExpression = "9/2"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 9 2
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 4.75 WHEN f(x) = 9.5/2" do
      let
        -- given
        rawExpression = "9.5/2"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 19 4
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 4.75 WHEN f(x) = 9.5/2" do
      let
        -- given
        rawExpression = "9.5/2"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 19 4
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 7.0 WHEN f(x) = 9-2" do
      let
        -- given
        rawExpression = "9-2"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 7 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 6.0 WHEN f(x) = 9.5-3.5" do
      let
        -- given
        rawExpression = "9.5-3.5"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 6 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 12.0 WHEN f(x) = (2+4)+6" do
      let
        -- given
        rawExpression = "(2+4)+6"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 12 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 36.0 WHEN f(x) = (2+4)*6" do
      let
        -- given
        rawExpression = "(2+4)*6"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 36 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 26.0 WHEN f(x) = 2+4*6" do
      let
        -- given
        rawExpression = "2+4*6"

        -- when
        result = parseAndEvaluate [] rawExpression

        expectedResult = ratioHelp 26 1
      -- then
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 8.0 WHEN f(x) = 2^3" do
      let
        -- given
        rawExpression = "2^3"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 8 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 1.0 WHEN f(x) = sin(pi/2)" do
      let
        -- given
        rawExpression = "sin(pi/2)"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 1 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = -1.0  WHEN f(x) = cos(pi)" do
      let
        -- given
        rawExpression = "cos(pi)"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp (-1) 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 1.0 WHEN f(x) = cos(0)" do
      let
        -- given
        rawExpression = "cos(0)"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 1 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 0.0 WHEN f(x) = tan(0)" do
      let
        -- given
        rawExpression = "tan(0)"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 0 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 1.0 WHEN f(x) = tan(pi/4)" do
      let
        -- given
        rawExpression = "tan(pi/4)"

        -- when
        result = parseAndEvaluate [] rawExpression

        -- then
        expectedResult = ratioHelp 1 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 2 WHEN f(x) = sqrt(4)" do
      let
        -- given
        rawExpression = "sqrt(4)"

        -- when
        result = parseAndEvaluate [] rawExpression

        expectedResult = ratioHelp 2 1
      -- then
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "SHOULD throw error 'Unsupported operation: log' WHEN f(x) = log(1)" do
      let
        -- given
        rawExpression = "log(1)"

        -- when
        result = fromExpect $ parseAndEvaluate [] rawExpression

        -- then
        expectedResult = "Unsupported operation: log"
      equal expectedResult result
    test "ASSERT f(x) = 5.0 WHEN f(x) = x AND x = 5.0" do
      let
        -- given
        x = 5.0

        variables = [ (Tuple "x" (ratioHelp 5 1)) ]

        rawExpression = "x"

        -- when
        result = parseAndEvaluate variables rawExpression

        -- then
        expectedResult = ratioHelp 5 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 4.0 WHEN f(x) = 2.0*x AND x = 2.0" do
      let
        -- given
        x = 2.0

        variables = [ (Tuple "x" (ratioHelp 2 1)) ]

        rawExpression = "2.0*x"

        -- when
        result = parseAndEvaluate variables rawExpression

        -- then
        expectedResult = ratioHelp 4 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 4.0 WHEN f(x) = 2*x AND x = 2.0" do
      let
        -- given
        x = 2.0

        variables = [ (Tuple "x" (ratioHelp 2 1)) ]

        rawExpression = "2.0*x"

        -- when
        result = parseAndEvaluate variables rawExpression

        -- then
        expectedResult = ratioHelp 4 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 1.0 WHEN f(x) = 1 / (1 + (100 * (x ^ 2))) AND x = 0.0" do
      let
        -- given
        x = 0.0

        variables = [ (Tuple "x" (ratioHelp 0 1)) ]

        rawExpression = "1 / (1 + (100 * (x ^ 2)))"

        -- when
        result = parseAndEvaluate variables rawExpression

        -- then
        expectedResult = ratioHelp 1 1
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "ASSERT f(x) = 0.5 WHEN f(x) = 1 / (1 + (100 * (x ^ 2))) AND x = 0.1" do
      let
        -- given
        x = 0.1

        variables = [ (Tuple "x" (ratioHelp 1 10)) ]

        rawExpression = "1 / (1 + (100 * (x ^ 2)))"

        -- when
        result = parseAndEvaluate variables rawExpression

        -- then
        expectedResult = ratioHelp 1 2
      expectValue result
        $ \value ->
            equal true $ consistent value expectedResult
    test "SHOULD throw error 'Unknown value: a | Unknown value: b' WHEN f(x) = a + b AND a is undefined AND b in undefined" do
      let
        -- given
        rawExpression = "a + b"

        -- when
        result = fromExpect $ parseAndEvaluate [] rawExpression

        -- then
        expectedResult = "Unknown value: a | Unknown value: b"
      equal expectedResult result

parseAndEvaluate :: VariableMap Approx -> String -> Expect Approx
parseAndEvaluate variableMap rawExpression = result
  where
  expressionOrParseError = parse rawExpression

  valueOrEvaluationError = case expressionOrParseError of
    Right expression -> evaluate variableMap expression
    Left error -> throw error

  result = valueOrEvaluationError

ratioHelp :: Int -> Int -> Approx
ratioHelp n d = fromRationalPrec 50 ((big n) % (big d))

fromExpect :: Expect Approx -> String
fromExpect (Right value) = show value

fromExpect (Left error) = show error
