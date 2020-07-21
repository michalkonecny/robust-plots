module Test.Expression.Evaluate.AutomaticDifferentiation.EvaluateDerivative
  ( evaluateDerivativeTests
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Expression.Error (Expect)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative, evaluateDerivative)
import Expression.Parser (parse)
import Math (e)
import Test.QuickCheck ((===))
import Test.TestUtils (equalTolerance)
import Test.Unit (Test, TestSuite, failure, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

evaluateDerivativeTests :: TestSuite
evaluateDerivativeTests =
  suite "Expression.Evaluate.AutomaticDifferentiation - evaluateDerivative" do
    test "ASSERT f(x)' = 1 WHEN f(x) = x" do
      let
        -- given
        rawExpression = "x"

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: 0.0, derivative: 1.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 0.0 1.0
      equalExpect expectedResult result
    test "ASSERT f(x)' = 0 WHEN f(x) = n FOR ANY integer n WHEN x = 0" $ quickCheck
      $ \(n :: Int) -> do
          let
            -- given
            value = toNumber n

            rawExpression = show $ value

            -- when
            result =
              fromExpect
                $ do
                    expression <- parse rawExpression
                    valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: 0.0, derivative: 1.0 } ] expression
                    pure valueAndDerivative

            -- then
            expectedResult = show $ toValueAndDerivative value 0.0
          expectedResult === result
    test "ASSERT f(x)' = 2*x WHEN f(x) = x^2 WHEN x = 3" do
      let
        -- given
        rawExpression = "x^2"

        x = 3.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: x, derivative: 1.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 9.0 6.0
      equalExpect expectedResult result
    test "ASSERT f(x)' = x+x WHEN f(x) = x*x WHEN x = 6" do
      let
        -- given
        rawExpression = "x*x"

        x = 6.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: x, derivative: 1.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 36.0 12.0
      equalExpect expectedResult result
    test "ASSERT f(x)' = (x*x)+((x+x)*x) = 3*(x^2) WHEN f(x) = x*x*x" do
      let
        -- given
        rawExpression = "x*x*x"

        x = 5.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: x, derivative: 1.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 125.0 75.0
      equalExpect expectedResult result
    test "ASSERT f(x)' = (-((100*x)+(100*x)))/((1+((100*x)*x))^2) = 3*(x^2) WHEN f(x) = 1/(1+(100*x*x))" do
      let
        -- given
        rawExpression = "1/(1+(100*x*x))"

        x = 3.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: x, derivative: 1.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 0.0011098779134295228 (-0.0007390973896312027)
      equalExpect expectedResult result
    test "ASSERT f(x)' = 12*x WHEN f(x) = 6*(x^2)" do
      let
        -- given
        rawExpression = "6*(x^2)"

        x = 3.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: x, derivative: 1.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 54.0 36.0
      equalExpect expectedResult result
    test "ASSERT f(x)' = (x^(x--1))*(x+((x*x)*(log(x)))) WHEN f(x) = x^x" do
      let
        -- given
        rawExpression = "x^x"

        x = 2.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: x, derivative: 1.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 4.0 6.772588722239782
      equalExpect expectedResult result
    test "ASSERT f(x)' = (-1)/(x^2) WHEN f(x) = 1/x" do
      let
        -- given
        rawExpression = "1/x"

        x = 2.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: x, derivative: 1.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 0.5 (-0.25)
      equalExpect expectedResult result
    test "ASSERT f(x)' = 3*(e^(3*x)) WHEN f(x) = e^(3*x)" do
      let
        -- given
        rawExpression = "e^(3*x)"

        x = 3.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: x, derivative: 1.0 }, Tuple "e" { value: e, derivative: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 8103.083927575384 24309.25178272615
      equalExpect expectedResult result
    test "ASSERT f(x)' = 2*(cos(2*x)) WHEN f(x) = sin(2*x)" do
      let
        -- given
        rawExpression = "sin(2*x)"

        x = 2.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: x, derivative: 1.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative (-0.7568024953079282) (-1.3072872417272239)
      equalExpect expectedResult result
    test "ASSERT f(x)' = -(2*(sin(2*x))) WHEN f(x) = cos(2*x)" do
      let
        -- given
        rawExpression = "cos(2*x)"

        x = 2.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: x, derivative: 1.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative (-0.6536436208636119) (1.5136049906158564)
      equalExpect expectedResult result
    test "ASSERT f(x)' = 2*(1+((tan(2*x))^2)) WHEN f(x) = tan(2*x)" do
      let
        -- given
        rawExpression = "tan(2*x)"

        x = 2.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: x, derivative: 1.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 1.1578212823495777 4.681100243723241
      equalExpect expectedResult result
    test "ASSERT f(x)' = 2/(2*x) WHEN f(x) = log(2*x)" do
      let
        -- given
        rawExpression = "log(2*x)"

        x = 2.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative [ Tuple "x" { value: x, derivative: 1.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 1.3862943611198906 0.5
      equalExpect expectedResult result

fromExpect :: Expect (ValueAndDerivative Number) -> String
fromExpect (Right value) = show value

fromExpect (Left error) = show error

toValueAndDerivative :: Number -> Number -> ValueAndDerivative Number
toValueAndDerivative value derivative = { value, derivative }

equalExpect :: Expect (ValueAndDerivative Number) -> Expect (ValueAndDerivative Number) -> Test
equalExpect (Left error1) (Left error2) = equal error1 error2

equalExpect (Left error1) (Right value) = failure $ "expected " <> show error1 <> ", got " <> show value

equalExpect (Right value) (Left error1) = failure $ "expected " <> show value <> ", got " <> show error1

equalExpect (Right expected) (Right actual) = do
  equalTolerance expected.value actual.value
  equalTolerance expected.derivative actual.derivative
