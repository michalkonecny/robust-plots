module Test.Expression.Evaluator.AutomaticDifferentiation.EvaluateDerivative
  ( evaluateDerivativeTests
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Expression.Error (Expect)
import Expression.Evaluator.AutomaticDifferentiator (evaluateDerivative)
import Expression.Parser (parse)
import Expression.Syntax (Expression)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

evaluateDerivativeTests :: TestSuite
evaluateDerivativeTests =
  suite "Expression.Evaluator.AutomaticDifferentiation - evaluateDerivative" do
    test "ASSERT f(x)' = 0 WHEN f(x) = 1" do
      let
        -- given
        rawExpression = "1"

        -- when
        result = fromExpect $ do 
          expr <- parseExpr rawExpression
          valueAndDerivative <- evaluateDerivative [Tuple "x" { value : 1.0, derivative: 1.0 } ] expr
          pure valueAndDerivative.derivative

        -- then
        expectedResult = "0.0"
      equal expectedResult result
    -- test "ASSERT f(x)' = 1 WHEN f(x) = x" do
    --   let
    --     -- given
    --     rawExpression = "x"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "1"
    --   equal expectedResult result
    -- test "ASSERT f(x)' = 2*x WHEN f(x) = x^2" do
    --   let
    --     -- given
    --     rawExpression = "x^2"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "2*x"
    --   equal expectedResult result
    -- test "ASSERT f(x)' = x+x WHEN f(x) = x*x" do
    --   let
    --     -- given
    --     rawExpression = "x*x"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "x+x"
    --   equal expectedResult result
    -- test "ASSERT f(x)' = (x*x)+((x+x)*x) = 3*(x^2) WHEN f(x) = x*x*x" do
    --   let
    --     -- given
    --     rawExpression = "x*x*x"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "(x*x)+((x+x)*x)"
    --   equal expectedResult result
    -- test "ASSERT f(x)' = (-((100*x)+(100*x)))/((1+((100*x)*x))^2) = 3*(x^2) WHEN f(x) = 1/(1+(100*x*x))" do
    --   let
    --     -- given
    --     rawExpression = "1/(1+(100*x*x))"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "(-((100*x)+(100*x)))/((1+((100*x)*x))^2)"
    --   equal expectedResult result
    -- test "ASSERT f(x)' = 12*x WHEN f(x) = 6*(x^2)" do
    --   let
    --     -- given
    --     rawExpression = "6*(x^2)"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "12*x"
    --   equal expectedResult result
    -- test "ASSERT f(x)' = (x^(x--1))*(x+((x*x)*(log(x)))) WHEN f(x) = x^x" do
    --   let
    --     -- given
    --     rawExpression = "x^x"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "(x^(x--1))*(x+((x*x)*(log(x))))"
    --   equal expectedResult result
    -- test "ASSERT f(x)' = (-1)/(x^2) WHEN f(x) = 1/x" do
    --   let
    --     -- given
    --     rawExpression = "1/x"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "(-1)/(x^2)"
    --   equal expectedResult result
    -- test "ASSERT f(x)' = 3*(e^(3*x)) WHEN f(x) = e^(3*x)" do
    --   let
    --     -- given
    --     rawExpression = "e^(3*x)"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "3*(e^(3*x))"
    --   equal expectedResult result
    -- test "ASSERT f(x)' = 0 WHEN f(x) = e^3" do
    --   let
    --     -- given
    --     rawExpression = "e^3"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "0"
    --   equal expectedResult result
    -- test "ASSERT f(x)' = 2*(cos(2*x)) WHEN f(x) = sin(2*x)" do
    --   let
    --     -- given
    --     rawExpression = "sin(2*x)"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "2*(cos(2*x))"
    --   equal expectedResult result
    -- test "ASSERT f(x)' = -(2*(sin(2*x))) WHEN f(x) = cos(2*x)" do
    --   let
    --     -- given
    --     rawExpression = "cos(2*x)"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "-(2*(sin(2*x)))"
    --   equal expectedResult result
    -- test "ASSERT f(x)' = 2*(1+((tan(2*x))^2)) WHEN f(x) = tan(2*x)" do
    --   let
    --     -- given
    --     rawExpression = "tan(2*x)"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "2*(1+((tan(2*x))^2))"
    --   equal expectedResult result
    -- test "ASSERT f(x)' = 2/(2*x) WHEN f(x) = log(2*x)" do
    --   let
    --     -- given
    --     rawExpression = "log(2*x)"

    --     -- when
    --     result = fromExpect $ parseAndDifferentiate rawExpression

    --     -- then
    --     expectedResult = "2/(2*x)"
    --   equal expectedResult result

parseExpr :: String -> Expect Expression
parseExpr rawExpression = expressionOrParseError
  where
  expressionOrParseError = parse rawExpression

fromExpect :: Expect Number -> String
fromExpect (Right value) = show value

fromExpect (Left error) = show error
