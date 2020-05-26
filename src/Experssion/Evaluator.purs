module Plot.Expression.Evaluator where

import Prelude

import Data.Array (find)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math (cos, e, exp, log, pi, pow, sin, sqrt, tan)
import Plot.Expression.Error (Expect, unknownValue)
import Plot.Expression.Syntax (BinaryOperation(..), Constant(..), Expression(..), UnaryOperation(..), VariableName)

type VariableMap a = Array (Tuple VariableName a)

lookup :: forall a. VariableMap a -> VariableName -> Maybe a
lookup variableMap variableName = toMaybeValue $ find search variableMap
  where 
    search :: Tuple VariableName a -> Boolean
    search (Tuple name _) = name == variableName

    toMaybeValue :: Maybe (Tuple VariableName a) -> Maybe a
    toMaybeValue (Just (Tuple _ value)) = Just value
    toMaybeValue _ = Nothing


evaluate :: VariableMap Number -> Expression -> Expect Expression
evaluate variableMap = case _ of
  expression@(ExpressionLiteral value) -> pure expression
  ExpressionConstant constant -> evaluateConstant constant
  ExpressionVariable name -> case lookup variableMap name of
    Just value -> pure $ ExpressionLiteral value
    _ -> unknownValue name
  ExpressionBinary operation leftExpression rightExpression -> evaluateBinaryOperation operation variableMap leftExpression rightExpression
  ExpressionUnary operation expression -> evaluateUnaryOperation operation variableMap expression

evaluateBinaryOperation :: BinaryOperation -> VariableMap Number -> Expression -> Expression -> Expect Expression
evaluateBinaryOperation Plus = evaluateArithmeticBinaryOperation add
evaluateBinaryOperation Minus = evaluateArithmeticBinaryOperation sub
evaluateBinaryOperation Times = evaluateArithmeticBinaryOperation mul
evaluateBinaryOperation Divide = evaluateArithmeticBinaryOperation div
evaluateBinaryOperation Power = evaluateArithmeticBinaryOperation pow

evaluateArithmeticBinaryOperation :: (Number -> Number -> Number) -> VariableMap Number -> Expression -> Expression -> Expect Expression
evaluateArithmeticBinaryOperation operation variableMap (ExpressionLiteral leftValue) (ExpressionLiteral rightValue) = pure $ ExpressionLiteral $ operation leftValue rightValue
evaluateArithmeticBinaryOperation operation variableMap leftExpression rightExpression = do
    leftValue <- evaluate variableMap leftExpression
    rightValue <- evaluate variableMap rightExpression
    evaluateArithmeticBinaryOperation operation variableMap leftValue rightValue

evaluateConstant :: Constant -> Expect Expression
evaluateConstant (Pi) = pure $ ExpressionLiteral pi
evaluateConstant (E) = pure $ ExpressionLiteral e

evaluateUnaryOperation :: UnaryOperation -> VariableMap Number -> Expression -> Expect Expression
evaluateUnaryOperation (Neg) = evaluateNegate 
evaluateUnaryOperation (Sqrt) = evaluateFunction sqrt
evaluateUnaryOperation (Exp) = evaluateFunction exp
evaluateUnaryOperation (Log) = evaluateFunction log
evaluateUnaryOperation (Sine) = evaluateFunction sin
evaluateUnaryOperation (Cosine) = evaluateFunction cos
evaluateUnaryOperation (Tan) = evaluateFunction tan

evaluateFunction :: (Number -> Number) -> VariableMap Number -> Expression -> Expect Expression
evaluateFunction op variableMap (ExpressionLiteral value) = pure $ ExpressionLiteral $ op value
evaluateFunction op variableMap expression = evaluate variableMap expression >>= evaluateNegate variableMap

evaluateNegate :: VariableMap Number -> Expression -> Expect Expression
evaluateNegate variableMap (ExpressionLiteral value) = pure $ ExpressionLiteral $ -value
evaluateNegate variableMap expression = evaluate variableMap expression >>= evaluateNegate variableMap