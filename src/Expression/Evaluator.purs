module Expression.Evaluator where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Expression.Error (Expect, multipleErrors, throw, unknownValue, unsupportedOperation)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))
import Expression.VariableMap (VariableMap, lookup)
import IntervalArith.Approx (Approx, fromRationalPrec)
import IntervalArith.Misc (rationalToNumber)
import Math (cos, exp, log, pow, sin, sqrt, tan, e, pi)
import Prelude (add, div, mul, negate, pure, show, sub, ($), (<>))

presetConstants :: Array (Tuple String Number)
presetConstants = [ (Tuple "pi" pi), (Tuple "e" e) ]

----------------------------------------------------
-- Evaluate Number
----------------------------------------------------
roughEvaluate :: VariableMap Number -> Expression -> Expect Number
roughEvaluate variableMap = case _ of
  ExpressionLiteral value -> pure $ rationalToNumber value
  ExpressionVariable name -> case lookup variableMap name of
    Just value -> pure value
    _ -> unknownValue name
  ExpressionBinary operation leftExpression rightExpression -> roughEvaluateBinaryOperation operation variableMap leftExpression rightExpression
  ExpressionUnary operation expression -> roughEvaluateUnaryOperation operation variableMap expression
  ExpressionLet name expression parentExpression -> case roughEvaluate variableMap expression of
    Right value -> roughEvaluate (variableMap <> [ (Tuple name value) ]) parentExpression
    Left error -> Left error

roughEvaluateBinaryOperation :: BinaryOperation -> VariableMap Number -> Expression -> Expression -> Expect Number
roughEvaluateBinaryOperation Plus = roughEvaluateArithmeticBinaryOperation add

roughEvaluateBinaryOperation Minus = roughEvaluateArithmeticBinaryOperation sub

roughEvaluateBinaryOperation Times = roughEvaluateArithmeticBinaryOperation mul

roughEvaluateBinaryOperation Divide = roughEvaluateArithmeticBinaryOperation div

roughEvaluateBinaryOperation Power = roughEvaluateArithmeticBinaryOperation pow

roughEvaluateArithmeticBinaryOperation :: (Number -> Number -> Number) -> VariableMap Number -> Expression -> Expression -> Expect Number
roughEvaluateArithmeticBinaryOperation operation variableMap leftExpression rightExpression = case roughEvaluate variableMap leftExpression, roughEvaluate variableMap rightExpression of
  Right leftValue, Right rightValue -> pure $ operation leftValue rightValue
  Left leftError, Left rightError -> multipleErrors $ (show leftError) <> " | " <> (show rightError)
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
  ExpressionLiteral value -> pure $ fromRationalPrec 4 value
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

evaluateBinaryOperation Power = (\_ _ _-> unsupportedOperation "pow")

evaluateArithmeticBinaryOperation :: (Approx -> Approx -> Approx) -> VariableMap Approx -> Expression -> Expression -> Expect Approx
evaluateArithmeticBinaryOperation operation variableMap leftExpression rightExpression = case evaluate variableMap leftExpression, evaluate variableMap rightExpression of
  Right leftValue, Right rightValue -> pure $ operation leftValue rightValue
  Left leftError, Left rightError -> multipleErrors $ (show leftError) <> " | " <> (show rightError)
  Left leftError, _ -> throw leftError
  _, Left rightError -> throw rightError

evaluateUnaryOperation :: UnaryOperation -> VariableMap Approx -> Expression -> Expect Approx
evaluateUnaryOperation (Neg) = evaluateNegate

evaluateUnaryOperation (Sqrt) = (\_ _-> unsupportedOperation "sqrt")

evaluateUnaryOperation (Exp) = (\_ _-> unsupportedOperation "exp")

evaluateUnaryOperation (Log) = (\_ _-> unsupportedOperation "log")

evaluateUnaryOperation (Sine) = (\_ _-> unsupportedOperation "sin")

evaluateUnaryOperation (Cosine) = (\_ _-> unsupportedOperation "cos")

evaluateUnaryOperation (Tan) = (\_ _-> unsupportedOperation "tan")

evaluateFunction :: (Approx -> Approx) -> VariableMap Approx -> Expression -> Expect Approx
evaluateFunction op variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ op value

evaluateNegate :: VariableMap Approx -> Expression -> Expect Approx
evaluateNegate variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ -value