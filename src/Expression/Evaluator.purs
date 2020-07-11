module Expression.Evaluator where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Expression.Error (Expect, evaluationError, multipleErrors, throw, unknownValue)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))
import Expression.VariableMap (VariableMap, lookup)
import IntervalArith.Approx (Approx, fromRationalPrec)
import IntervalArith.Approx.ExpLog (eA, expA, logA, powA)
import IntervalArith.Approx.Pi (piA)
import IntervalArith.Approx.SinCos (cosA, sinA, tanA)
import IntervalArith.Approx.Sqrt (sqrtA)
import IntervalArith.Misc (rationalToNumber)
import Math (cos, e, exp, log, pi, pow, sin, sqrt, tan)

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
  ExpressionLiteral value -> pure $ fromRationalPrec precision value
  ExpressionVariable name -> case lookup (presetConstants <> variableMap) name of
    Just value -> pure value
    _ -> unknownValue name
  ExpressionBinary operation leftExpression rightExpression -> evaluateBinaryOperation operation variableMap leftExpression rightExpression
  ExpressionUnary operation expression -> evaluateUnaryOperation operation variableMap expression
  ExpressionLet name expression parentExpression -> case evaluate variableMap expression of
    Right value -> evaluate (variableMap <> [ (Tuple name value) ]) parentExpression
    Left error -> Left error
  where
  precision = 50

  presetConstants = [ (Tuple "pi" (piA precision)), (Tuple "e" (eA precision)) ]

evaluateBinaryOperation :: BinaryOperation -> VariableMap Approx -> Expression -> Expression -> Expect Approx
evaluateBinaryOperation Plus = evaluateArithmeticBinaryOperation add

evaluateBinaryOperation Minus = evaluateArithmeticBinaryOperation sub

evaluateBinaryOperation Times = evaluateArithmeticBinaryOperation mul

evaluateBinaryOperation Divide = evaluateArithmeticBinaryOperation div

evaluateBinaryOperation Power = evaluateArithmeticPartialBinaryOperation powA

evaluateArithmeticBinaryOperation :: (Approx -> Approx -> Approx) -> VariableMap Approx -> Expression -> Expression -> Expect Approx
evaluateArithmeticBinaryOperation operation variableMap leftExpression rightExpression = case evaluate variableMap leftExpression, evaluate variableMap rightExpression of
  Right leftValue, Right rightValue -> pure $ operation leftValue rightValue
  Left leftError, Left rightError -> multipleErrors [ leftError, rightError ]
  Left leftError, _ -> throw leftError
  _, Left rightError -> throw rightError

evaluateArithmeticPartialBinaryOperation :: (Approx -> Approx -> Maybe Approx) -> VariableMap Approx -> Expression -> Expression -> Expect Approx
evaluateArithmeticPartialBinaryOperation operation variableMap leftExpression rightExpression = case evaluate variableMap leftExpression, evaluate variableMap rightExpression of
  Right leftValue, Right rightValue -> case operation leftValue rightValue of
    Just result -> pure result
    _ -> evaluationError "pow: a parameter is out of range"
  Left leftError, Left rightError -> multipleErrors [ leftError, rightError ]
  Left leftError, _ -> throw leftError
  _, Left rightError -> throw rightError

evaluateUnaryOperation :: UnaryOperation -> VariableMap Approx -> Expression -> Expect Approx
evaluateUnaryOperation (Neg) = evaluateNegate

evaluateUnaryOperation (Sqrt) = evaluateSqrt

evaluateUnaryOperation (Exp) = evaluateExp

evaluateUnaryOperation (Log) = evaluateLog

evaluateUnaryOperation (Sine) = evaluateSine

evaluateUnaryOperation (Cosine) = evaluateCosine

evaluateUnaryOperation (Tan) = evaluateTan

evaluateNegate :: VariableMap Approx -> Expression -> Expect Approx
evaluateNegate variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ -value

evaluateSqrt :: VariableMap Approx -> Expression -> Expect Approx
evaluateSqrt variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> case sqrtA value of
    Just result -> pure result
    _ -> evaluationError "sqrt: parameter out of range"

evaluateLog :: VariableMap Approx -> Expression -> Expect Approx
evaluateLog variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> case logA value of
    Just result -> pure result
    _ -> evaluationError "log: parameter out of range"

evaluateExp :: VariableMap Approx -> Expression -> Expect Approx
evaluateExp variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ expA value

evaluateSine :: VariableMap Approx -> Expression -> Expect Approx
evaluateSine variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ sinA value

evaluateCosine :: VariableMap Approx -> Expression -> Expect Approx
evaluateCosine variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ cosA value

evaluateTan :: VariableMap Approx -> Expression -> Expect Approx
evaluateTan variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ tanA value
