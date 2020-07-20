module Test.Expression.Evaluator.RoughEvaluate
  ( roughEvaluateTests
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Expression.Error (Expect, throw)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative, evaluateDerivativeWithSample)
import Expression.Parser (parse)
import Expression.VariableMap (VariableMap)
import Test.QuickCheck ((===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

roughEvaluateTests :: TestSuite
roughEvaluateTests =
  suite "Expression.Evaluator - roughEvaluate" do
    test "ASSERT f(x) = n WHEN f(x) = n FOR ANY integer n" $ quickCheck
      $ \(n :: Int) -> do
          let
            -- given
            variables = []

            rawExpression = show $ toNumber n

            -- when
            result = fromExpect $ parseAndEvaluate variables rawExpression

            -- then
            expectedResult = show $ toNumber n
          expectedResult === result
    test "ASSERT f(x) = 9.0 WHEN f(x) = 4+5" do
      let
        -- given
        variables = []

        rawExpression = "4+5"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 9.0
      equal expectedResult result
    test "ASSERT f(x) = 7.5 WHEN f(x) = 4.5+3" do
      let
        -- given
        variables = []

        rawExpression = "4.5+3"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 7.5
      equal expectedResult result
    test "ASSERT f(x) = 12.0 WHEN f(x) = 4*3" do
      let
        -- given
        variables = []

        rawExpression = "4*3"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 12.0
      equal expectedResult result
    test "ASSERT f(x) = 9.0 WHEN f(x) = 4.5*2" do
      let
        -- given
        variables = []

        rawExpression = "4.5*2"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 9.0
      equal expectedResult result
    test "ASSERT f(x) = 4.0 WHEN f(x) = 8/2" do
      let
        -- given
        variables = []

        rawExpression = "8/2"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 4.0
      equal expectedResult result
    test "ASSERT f(x) = 4.5 WHEN f(x) = 9/2" do
      let
        -- given
        variables = []

        rawExpression = "9/2"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 4.5
      equal expectedResult result
    test "ASSERT f(x) = 4.75 WHEN f(x) = 9.5/2" do
      let
        -- given
        variables = []

        rawExpression = "9.5/2"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 4.75
      equal expectedResult result
    test "ASSERT f(x) = 4.75 WHEN f(x) = 9.5/2" do
      let
        -- given
        variables = []

        rawExpression = "9.5/2"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 4.75
      equal expectedResult result
    test "ASSERT f(x) = 7.0 WHEN f(x) = 9-2" do
      let
        -- given
        variables = []

        rawExpression = "9-2"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 7.0
      equal expectedResult result
    test "ASSERT f(x) = 6.0 WHEN f(x) = 9.5-3.5" do
      let
        -- given
        variables = []

        rawExpression = "9.5-3.5"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 6.0
      equal expectedResult result
    test "ASSERT f(x) = 12.0 WHEN f(x) = (2+4)+6" do
      let
        -- given
        variables = []

        rawExpression = "(2+4)+6"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 12.0
      equal expectedResult result
    test "ASSERT f(x) = 36.0 WHEN f(x) = (2+4)*6" do
      let
        -- given
        variables = []

        rawExpression = "(2+4)*6"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 36.0
      equal expectedResult result
    test "ASSERT f(x) = 26.0 WHEN f(x) = 2+4*6" do
      let
        -- given
        variables = []

        rawExpression = "2+4*6"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 26.0
      equal expectedResult result
    test "ASSERT f(x) = 8.0 WHEN f(x) = 2^3" do
      let
        -- given
        variables = []

        rawExpression = "2^3"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 8.0
      equal expectedResult result
    test "ASSERT f(x) = 1.0 WHEN f(x) = sin(pi/2)" do
      let
        -- given
        variables = []

        rawExpression = "sin(pi/2)"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 1.0
      equal expectedResult result
    test "ASSERT f(x) = 0.0 WHEN f(x) = sin(0)" do
      let
        -- given
        variables = []

        rawExpression = "sin(0)"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 0.0
      equal expectedResult result
    test "ASSERT f(x) = -1.0 WHEN f(x) = cos(pi)" do
      let
        -- given
        variables = []

        rawExpression = "cos(pi)"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show $ -1.0
      equal expectedResult result
    test "ASSERT f(x) = 1.0 WHEN f(x) = cos(0)" do
      let
        -- given
        variables = []

        rawExpression = "cos(0)"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 1.0
      equal expectedResult result
    test "ASSERT f(x) = 0.0 WHEN f(x) = tan(0)" do
      let
        -- given
        variables = []

        rawExpression = "tan(0)"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 0.0
      equal expectedResult result
    test "ASSERT f(x) = 2.0 WHEN f(x) = sqrt(4)" do
      let
        -- given
        variables = []

        rawExpression = "sqrt(4)"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 2.0
      equal expectedResult result
    test "ASSERT f(x) = 0.0 WHEN f(x) = log(1)" do
      let
        -- given
        variables = []

        rawExpression = "log(1)"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 0.0
      equal expectedResult result
    test "ASSERT f(x) = 5.0 WHEN f(x) = x AND x = 5.0" do
      let
        -- given
        x = 5.0

        variables = [ (Tuple "x" { value: x, derivative: zero }) ]

        rawExpression = "x"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 5.0
      equal expectedResult result
    test "ASSERT f(x) = 4.0 WHEN f(x) = 2.0*x AND x = 2.0" do
      let
        -- given
        x = 2.0

        variables = [ (Tuple "x" { value: x, derivative: zero }) ]

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

        variables = [ (Tuple "x" { value: x, derivative: zero }) ]

        rawExpression = "2.0*x"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 4.0
      equal expectedResult result
    test "ASSERT f(x) = 1.0 WHEN f(x) = 1 / (1 + (100 * (x ^ 2))) AND x = 0.0" do
      let
        -- given
        x = 0.0

        variables = [ (Tuple "x" { value: x, derivative: zero }) ]

        rawExpression = "1 / (1 + (100 * (x ^ 2)))"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 1.0
      equal expectedResult result
    test "ASSERT f(x) = 0.5 WHEN f(x) = 1 / (1 + (100 * (x ^ 2))) AND x = 0.1" do
      let
        -- given
        x = 0.1

        variables = [ (Tuple "x" { value: x, derivative: zero }) ]

        rawExpression = "1 / (1 + (100 * (x ^ 2)))"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 0.5
      equal expectedResult result
    test "ASSERT f(x) = 0.5 WHEN f(x) = 1-(x/6)*(1-(x/20)) AND x = 0.0" do
      let
        -- given
        x = 0.0

        variables = [ (Tuple "x" { value: x, derivative: zero }) ]

        rawExpression = "1-(x/6)*(1-(x/20))"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = show 1.0
      equal expectedResult result
    test "SHOULD throw error 'Unknown value: x' WHEN f(x) = 1 / (1 + (100 * (x ^ 2))) AND x is undefined" do
      let
        -- given
        variables = []

        rawExpression = "1 / (1 + (100 * (x ^ 2)))"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = "Unknown value: x"
      equal expectedResult result
    test "SHOULD throw error 'Unknown value: a' WHEN f(x) = a + b AND a is undefined AND b in undefined" do
      let
        -- given
        variables = []

        rawExpression = "a + b"

        -- when
        result = fromExpect $ parseAndEvaluate variables rawExpression

        -- then
        expectedResult = "Unknown value: a"
      equal expectedResult result

parseAndEvaluate :: VariableMap (ValueAndDerivative Number) -> String -> Expect Number
parseAndEvaluate variables rawExpression = result
  where
  expressionOrParseError = parse rawExpression

  valueOrEvaluationError = case expressionOrParseError of
    Right expression -> evaluateDerivativeWithSample variables zero expression <#> (_.value)
    Left error -> throw error

  result = valueOrEvaluationError

fromExpect :: Expect Number -> String
fromExpect (Right value) = show value

fromExpect (Left error) = show error
