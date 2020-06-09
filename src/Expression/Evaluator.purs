module Expression.Evaluator where

import Prelude (add, div, mul, negate, pure, show, sub, ($), (<>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Expression.Error (Expect, unknownValue, multipleErrors, throw)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))
import Expression.EvaluationResult (EvaluationResult(..), cos, exp, log, pow, sin, sqrt, tan, e, pi)
import Expression.VariableMap (VariableMap, lookup)

presetConstants :: Array (Tuple String EvaluationResult)
presetConstants = [ (Tuple "pi" pi), (Tuple "e" e) ]

evaluate :: VariableMap EvaluationResult -> Expression -> Expect EvaluationResult
evaluate variableMap = case _ of
  ExpressionLiteral value -> pure $ Rational value
  ExpressionVariable name -> case lookup variableMap name of
    Just value -> pure value
    _ -> unknownValue name
  ExpressionBinary operation leftExpression rightExpression -> evaluateBinaryOperation operation variableMap leftExpression rightExpression
  ExpressionUnary operation expression -> evaluateUnaryOperation operation variableMap expression

evaluateBinaryOperation :: BinaryOperation -> VariableMap EvaluationResult -> Expression -> Expression -> Expect EvaluationResult
evaluateBinaryOperation Plus = evaluateArithmeticBinaryOperation add

evaluateBinaryOperation Minus = evaluateArithmeticBinaryOperation sub

evaluateBinaryOperation Times = evaluateArithmeticBinaryOperation mul

evaluateBinaryOperation Divide = evaluateArithmeticBinaryOperation div

evaluateBinaryOperation Power = evaluateArithmeticBinaryOperation pow

evaluateArithmeticBinaryOperation :: (EvaluationResult -> EvaluationResult -> EvaluationResult) -> VariableMap EvaluationResult -> Expression -> Expression -> Expect EvaluationResult
evaluateArithmeticBinaryOperation operation variableMap leftExpression rightExpression = case evaluate variableMap leftExpression, evaluate variableMap rightExpression of
  Right leftValue, Right rightValue -> pure $ operation leftValue rightValue
  Left leftError, Left rightError -> multipleErrors $ (show leftError) <> " | " <> (show rightError)
  Left leftError, _ -> throw leftError
  _, Left rightError -> throw rightError

evaluateUnaryOperation :: UnaryOperation -> VariableMap EvaluationResult -> Expression -> Expect EvaluationResult
evaluateUnaryOperation (Neg) = evaluateNegate

evaluateUnaryOperation (Sqrt) = evaluateFunction sqrt

evaluateUnaryOperation (Exp) = evaluateFunction exp

evaluateUnaryOperation (Log) = evaluateFunction log

evaluateUnaryOperation (Sine) = evaluateFunction sin

evaluateUnaryOperation (Cosine) = evaluateFunction cos

evaluateUnaryOperation (Tan) = evaluateFunction tan

evaluateFunction :: (EvaluationResult -> EvaluationResult) -> VariableMap EvaluationResult -> Expression -> Expect EvaluationResult
evaluateFunction op variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ op value

evaluateNegate :: VariableMap EvaluationResult -> Expression -> Expect EvaluationResult
evaluateNegate variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ -value
