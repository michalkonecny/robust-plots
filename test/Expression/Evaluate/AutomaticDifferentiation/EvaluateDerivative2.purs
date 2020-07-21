module Test.Expression.Evaluate.AutomaticDifferentiation.EvaluateDerivative2
  ( evaluateDerivative2Tests
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Expression.Error (Expect)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative2, evaluateDerivative2)
import Expression.Parser (parse)
import Math (e)
import Test.QuickCheck ((===))
import Test.TestUtils (equalTolerance)
import Test.Unit (Test, TestSuite, failure, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

evaluateDerivative2Tests :: TestSuite
evaluateDerivative2Tests =
  suite "Expression.Evaluate.AutomaticDifferentiation - evaluateDerivative2" do
    test "ASSERT f(x)' = 1 WHEN f(x) = x" do
      let
        -- given
        rawExpression = "x"

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: 0.0, derivative: 1.0, derivative2: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 0.0 1.0 0.0
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
                    valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: 0.0, derivative: 1.0, derivative2: 0.0 } ] expression
                    pure valueAndDerivative

            -- then
            expectedResult = show $ toValueAndDerivative value 0.0 0.0
          expectedResult === result
    test "ASSERT f(x)' = 2*x WHEN f(x) = x^2 WHEN x = 3" do
      let
        -- given
        rawExpression = "x^2"

        x = 3.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: x, derivative: 1.0, derivative2: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 9.0 6.0 2.0
      equalExpect expectedResult result
    test "ASSERT f(x)' = x+x WHEN f(x) = x*x WHEN x = 6" do
      let
        -- given
        rawExpression = "x*x"

        x = 6.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: x, derivative: 1.0, derivative2: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 36.0 12.0 2.0
      equalExpect expectedResult result
    test "ASSERT f(x)' = (x*x)+((x+x)*x) = 3*(x^2) WHEN f(x) = x*x*x" do
      let
        -- given
        rawExpression = "x*x*x"

        x = 5.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: x, derivative: 1.0, derivative2: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 125.0 75.0 30.0
      equalExpect expectedResult result
    test "ASSERT f(x)' = (-((100*x)+(100*x)))/((1+((100*x)*x))^2) = 3*(x^2) WHEN f(x) = 1/(1+(100*x*x))" do
      let
        -- given
        rawExpression = "1/(1+(100*x*x))"

        x = 3.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: x, derivative: 1.0, derivative2: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 0.0011098779134295228 (-0.0007390973896312027) 0.0007380036458063693
      equalExpect expectedResult result
    test "ASSERT f(x)' = 12*x WHEN f(x) = 6*(x^2)" do
      let
        -- given
        rawExpression = "6*(x^2)"

        x = 3.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: x, derivative: 1.0, derivative2: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 54.0 36.0 12.0
      equalExpect expectedResult result
    test "ASSERT f(x)' = (x^(x--1))*(x+((x*x)*(log(x)))) WHEN f(x) = x^x" do
      let
        -- given
        rawExpression = "x^x"

        x = 2.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: x, derivative: 1.0, derivative2: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 4.0 6.772588722239782 13.46698950015237
      equalExpect expectedResult result
    test "ASSERT f(x)' = (-1)/(x^2) AND f(x)'' = 2/(x^3) WHEN f(x) = 1/x" do
      let
        -- given
        rawExpression = "1/x"

        x = 2.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: x, derivative: 1.0, derivative2: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 0.5 (-0.25) 0.25
      equalExpect expectedResult result
    test "ASSERT f(x)' = 3*(e^(3*x)) WHEN f(x) = e^(3*x)" do
      let
        -- given
        rawExpression = "e^(3*x)"

        x = 3.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: x, derivative: 1.0, derivative2: 0.0 }, Tuple "e" { value: e, derivative: 0.0, derivative2: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 8103.083927575384 24309.25178272615 72927.75534817843
      equalExpect expectedResult result
    test "ASSERT f(x)' = 2*(cos(2*x)) WHEN f(x) = sin(2*x)" do
      let
        -- given
        rawExpression = "sin(2*x)"

        x = 2.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: x, derivative: 1.0, derivative2: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative (-0.7568024953079282) (-1.3072872417272239) 3.027209981231713
      equalExpect expectedResult result
    test "ASSERT f(x)' = -(2*(sin(2*x))) WHEN f(x) = cos(2*x)" do
      let
        -- given
        rawExpression = "cos(2*x)"

        x = 2.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: x, derivative: 1.0, derivative2: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative (-0.6536436208636119) (1.5136049906158564) 2.6145744834544478
      equalExpect expectedResult result
    test "ASSERT f(x)' = 2*(1+((tan(2*x))^2)) WHEN f(x) = tan(2*x)" do
      let
        -- given
        rawExpression = "tan(2*x)"

        x = 2.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: x, derivative: 1.0, derivative2: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 1.1578212823495777 4.681100243723241 21.679509947978254
      equalExpect expectedResult result
    test "ASSERT f(x)' = 2/(2*x) WHEN f(x) = log(2*x)" do
      let
        -- given
        rawExpression = "log(2*x)"

        x = 2.0

        -- when
        result = do
          expression <- parse rawExpression
          valueAndDerivative <- evaluateDerivative2 [ Tuple "x" { value: x, derivative: 1.0, derivative2: 0.0 } ] expression
          pure valueAndDerivative

        -- then
        expectedResult = pure $ toValueAndDerivative 1.3862943611198906 0.5 (-0.25)
      equalExpect expectedResult result

fromExpect :: Expect (ValueAndDerivative2 Number) -> String
fromExpect (Right value) = show value

fromExpect (Left error) = show error

toValueAndDerivative :: Number -> Number -> Number -> ValueAndDerivative2 Number
toValueAndDerivative value derivative derivative2 = { value, derivative, derivative2 }

equalExpect :: Expect (ValueAndDerivative2 Number) -> Expect (ValueAndDerivative2 Number) -> Test
equalExpect (Left error1) (Left error2) = equal error1 error2

equalExpect (Left error1) (Right value) = failure $ "expected " <> show error1 <> ", got " <> show value

equalExpect (Right value) (Left error1) = failure $ "expected " <> show value <> ", got " <> show error1

equalExpect (Right expected) (Right actual) = do
  equalTolerance expected.value actual.value
  equalTolerance expected.derivative actual.derivative
  equalTolerance expected.derivative2 actual.derivative2
