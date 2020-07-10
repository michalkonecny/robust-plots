module Expression.Evaluator where

import Prelude
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Expression.Error (Expect, evaluationError, multipleErrors, throw, unknownValue, unsupportedOperation)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))
import Expression.VariableMap (VariableMap, lookup)
import IntervalArith.Approx (Approx, fromRationalPrec)
import IntervalArith.Approx.Sqrt (sqrtA)
import IntervalArith.Misc (Rational, multiplicativePowerRecip, rationalToNumber)
import Math (cos, exp, log, pow, sin, sqrt, tan, e, pi)

----------------------------------------------------
-- Evaluate Number
----------------------------------------------------
roughEvaluate :: VariableMap Number -> Expression -> Expect Number
roughEvaluate variableMap = case _ of
  ExpressionLiteral value -> pure $ rationalToNumber value
  ExpressionVariable name -> case lookup (presetConstants <> variableMap) name of
    Just value -> pure value
    _ -> unknownValue name
  ExpressionBinary operation leftExpression rightExpression -> roughEvaluateBinaryOperation operation variableMap leftExpression rightExpression
  ExpressionUnary operation expression -> roughEvaluateUnaryOperation operation variableMap expression
  ExpressionLet name expression parentExpression -> case roughEvaluate variableMap expression of
    Right value -> roughEvaluate (variableMap <> [ (Tuple name value) ]) parentExpression
    Left error -> Left error
  where
  presetConstants :: Array (Tuple String Number)
  presetConstants = [ (Tuple "pi" pi), (Tuple "e" e) ]

roughEvaluateBinaryOperation :: BinaryOperation -> VariableMap Number -> Expression -> Expression -> Expect Number
roughEvaluateBinaryOperation Plus = roughEvaluateArithmeticBinaryOperation add

roughEvaluateBinaryOperation Minus = roughEvaluateArithmeticBinaryOperation sub

roughEvaluateBinaryOperation Times = roughEvaluateArithmeticBinaryOperation mul

roughEvaluateBinaryOperation Divide = roughEvaluateArithmeticBinaryOperation div

roughEvaluateBinaryOperation Power = roughEvaluateArithmeticBinaryOperation pow

roughEvaluateArithmeticBinaryOperation :: (Number -> Number -> Number) -> VariableMap Number -> Expression -> Expression -> Expect Number
roughEvaluateArithmeticBinaryOperation operation variableMap leftExpression rightExpression = case roughEvaluate variableMap leftExpression, roughEvaluate variableMap rightExpression of
  Right leftValue, Right rightValue -> pure $ operation leftValue rightValue
  Left leftError, Left rightError -> multipleErrors [ leftError, rightError ]
  Left leftError, _ -> throw leftError
  _, Left rightError -> throw rightError

roughEvaluateUnaryOperation :: UnaryOperation -> VariableMap Number -> Expression -> Expect Number
roughEvaluateUnaryOperation (Neg) = roughEvaluateNegate

roughEvaluateUnaryOperation (Sqrt) = roughEvaluateFunction sqrt

roughEvaluateUnaryOperation (Exp) = roughEvaluateFunction exp

roughEvaluateUnaryOperation (Log) = roughEvaluateFunction log

roughEvaluateUnaryOperation (Sine) = roughEvaluateFunction sin

roughEvaluateUnaryOperation (Cosine) = roughEvaluateFunction cos

roughEvaluateUnaryOperation (Tan) = roughEvaluateFunction tan

roughEvaluateFunction :: (Number -> Number) -> VariableMap Number -> Expression -> Expect Number
roughEvaluateFunction op variableMap expression = case roughEvaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ op value

roughEvaluateNegate :: VariableMap Number -> Expression -> Expect Number
roughEvaluateNegate variableMap expression = case roughEvaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ -value

----------------------------------------------------
-- Evaluate Approx
----------------------------------------------------
evaluate :: VariableMap Approx -> Expression -> Expect Approx
evaluate variableMap = case _ of
  ExpressionLiteral value -> pure $ fromRationalPrec 50 value
  ExpressionVariable name -> case lookup variableMap name of
    Just value -> pure value
    _ -> unknownValue name
  ExpressionBinary operation leftExpression rightExpression -> evaluateBinaryOperation operation variableMap leftExpression rightExpression
  ExpressionUnary operation expression -> evaluateUnaryOperation operation variableMap expression
  ExpressionLet name expression parentExpression -> case evaluate variableMap expression of
    Right value -> evaluate (variableMap <> [ (Tuple name value) ]) parentExpression
    Left error -> Left error

evaluateBinaryOperation :: BinaryOperation -> VariableMap Approx -> Expression -> Expression -> Expect Approx
evaluateBinaryOperation Plus = evaluateArithmeticBinaryOperation add

evaluateBinaryOperation Minus = evaluateArithmeticBinaryOperation sub

evaluateBinaryOperation Times = evaluateArithmeticBinaryOperation mul

evaluateBinaryOperation Divide = evaluateArithmeticBinaryOperation div

evaluateBinaryOperation Power = evaluatePow

evaluateArithmeticBinaryOperation :: (Approx -> Approx -> Approx) -> VariableMap Approx -> Expression -> Expression -> Expect Approx
evaluateArithmeticBinaryOperation operation variableMap leftExpression rightExpression = case evaluate variableMap leftExpression, evaluate variableMap rightExpression of
  Right leftValue, Right rightValue -> pure $ operation leftValue rightValue
  Left leftError, Left rightError -> multipleErrors [ leftError, rightError ]
  Left leftError, _ -> throw leftError
  _, Left rightError -> throw rightError

evaluateUnaryOperation :: UnaryOperation -> VariableMap Approx -> Expression -> Expect Approx
evaluateUnaryOperation (Neg) = evaluateNegate

evaluateUnaryOperation (Sqrt) = evaluateSqrt

evaluateUnaryOperation (Exp) = (\_ _ -> unsupportedOperation "exp")

evaluateUnaryOperation (Log) = (\_ _ -> unsupportedOperation "log")

evaluateUnaryOperation (Sine) = (\_ _ -> unsupportedOperation "sin")

evaluateUnaryOperation (Cosine) = (\_ _ -> unsupportedOperation "cos")

evaluateUnaryOperation (Tan) = (\_ _ -> unsupportedOperation "tan")

evaluateNegate :: VariableMap Approx -> Expression -> Expect Approx
evaluateNegate variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ -value

evaluateSqrt :: VariableMap Approx -> Expression -> Expect Approx
evaluateSqrt variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> case sqrtA value of
    Just result -> pure result
    _ -> evaluationError "sqrt parameter out of range"

evaluatePow :: VariableMap Approx -> Expression -> Expression -> Expect Approx
evaluatePow variableMap expression (ExpressionLiteral value) = case asInteger value of
  Just power -> case evaluate variableMap expression of
    Right expressionResult -> pure $ multiplicativePowerRecip expressionResult power
    Left error -> throw error
  Nothing -> powError

evaluatePow _ _ _ = powError

powError :: Expect Approx
powError = evaluationError "'^' only supported with positive integer powers"

asInteger :: Rational -> Maybe Int
asInteger value = if numberValue == toNumber integerValue then Just integerValue else Nothing
  where
  numberValue = rationalToNumber value

  integerValue = round $ numberValue
