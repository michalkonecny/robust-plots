module Expression.Evaluator where

import Prelude

import Data.Array (find)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Expression.Error (Expect, unknownValue, multipleErrors, throw)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..), VariableName)
import Math (cos, exp, log, pow, sin, sqrt, tan, e, pi)

type VariableMap a = Array (Tuple VariableName a)

lookup :: forall a. VariableMap a -> VariableName -> Maybe a
lookup variableMap variableName = toMaybeValue $ find search variableMap
  where 
    search :: Tuple VariableName a -> Boolean
    search (Tuple name _) = name == variableName

    toMaybeValue :: Maybe (Tuple VariableName a) -> Maybe a
    toMaybeValue (Just (Tuple _ value)) = Just value
    toMaybeValue _ = Nothing

presetConstants :: Array (Tuple String Number)
presetConstants = [ (Tuple "pi" pi), (Tuple "e" e) ]

evaluate :: VariableMap Number -> Expression -> Expect Number
evaluate variableMap = case _ of
  (ExpressionLiteral value) -> pure value
  ExpressionVariable name -> case lookup variableMap name of
    Just value -> pure value
    _ -> unknownValue name
  ExpressionBinary operation leftExpression rightExpression -> evaluateBinaryOperation operation variableMap leftExpression rightExpression
  ExpressionUnary operation expression -> evaluateUnaryOperation operation variableMap expression

evaluateBinaryOperation :: BinaryOperation -> VariableMap Number -> Expression -> Expression -> Expect Number
evaluateBinaryOperation Plus = evaluateArithmeticBinaryOperation add
evaluateBinaryOperation Minus = evaluateArithmeticBinaryOperation sub
evaluateBinaryOperation Times = evaluateArithmeticBinaryOperation mul
evaluateBinaryOperation Divide = evaluateArithmeticBinaryOperation div
evaluateBinaryOperation Power = evaluateArithmeticBinaryOperation pow

evaluateArithmeticBinaryOperation :: (Number -> Number -> Number) -> VariableMap Number -> Expression -> Expression -> Expect Number
evaluateArithmeticBinaryOperation operation variableMap leftExpression rightExpression = case evaluate variableMap leftExpression, evaluate variableMap rightExpression of
  Right leftValue, Right rightValue -> pure $ operation leftValue rightValue
  Left leftError, Left rightError -> multipleErrors $ (show leftError) <> " | " <> (show rightError)
  Left leftError, _ -> throw leftError
  _, Left rightError -> throw rightError

evaluateUnaryOperation :: UnaryOperation -> VariableMap Number -> Expression -> Expect Number
evaluateUnaryOperation (Neg) = evaluateNegate 
evaluateUnaryOperation (Sqrt) = evaluateFunction sqrt
evaluateUnaryOperation (Exp) = evaluateFunction exp
evaluateUnaryOperation (Log) = evaluateFunction log
evaluateUnaryOperation (Sine) = evaluateFunction sin
evaluateUnaryOperation (Cosine) = evaluateFunction cos
evaluateUnaryOperation (Tan) = evaluateFunction tan

evaluateFunction :: (Number -> Number) -> VariableMap Number -> Expression -> Expect Number
evaluateFunction op variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ op value

evaluateNegate :: VariableMap Number -> Expression -> Expect Number
evaluateNegate variableMap expression = case evaluate variableMap expression of
  Left error -> throw error
  Right value -> pure $ -value