module Expression.Simplifier where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Alt ((<|>))
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))

simplify :: Expression -> Expression
simplify (ExpressionUnary operation expression) = case simplify expression, operation of
  ExpressionLiteral value, Neg -> ExpressionLiteral (-value)
  simplifiedExpression, _ -> ExpressionUnary operation simplifiedExpression

simplify (ExpressionBinary Power leftExpression (ExpressionLiteral 1.0)) = simplify leftExpression

simplify (ExpressionBinary operation leftExpression rightExpression) = fromMaybe default $ withTrimmedLeafNodes <|> withSimplifedConstantLeafNodes <|> withSimplifedCommutativeOperations
  where
  simplifiedLeftExpression = simplify leftExpression

  simplifiedRightExpression = simplify rightExpression

  default = (ExpressionBinary operation simplifiedLeftExpression simplifiedRightExpression)

  withTrimmedLeafNodes = trimZeroLeafNodes simplifiedLeftExpression simplifiedRightExpression operation

  withSimplifedConstantLeafNodes = simplifyConstantLeafNodes simplifiedLeftExpression simplifiedRightExpression operation

  withSimplifedCommutativeOperations = simplifyCommutativeOperations simplifiedLeftExpression simplifiedRightExpression operation

simplify expression = expression

trimZeroLeafNodes :: Expression -> Expression -> BinaryOperation -> Maybe Expression
trimZeroLeafNodes simplifiedLeftExpression simplifiedRightExpression operation = case simplifiedLeftExpression, simplifiedRightExpression, operation of
  ExpressionLiteral 0.0, _, Times -> Just $ ExpressionLiteral 0.0
  _, ExpressionLiteral 0.0, Times -> Just $ ExpressionLiteral 0.0
  ExpressionLiteral 0.0, _, Plus -> Just $ simplifiedRightExpression
  _, ExpressionLiteral 0.0, Plus -> Just $ simplifiedLeftExpression
  ExpressionLiteral 0.0, _, Minus -> Just $ ExpressionUnary Neg simplifiedRightExpression
  _, ExpressionLiteral 0.0, Minus -> Just $ simplifiedLeftExpression
  ExpressionLiteral 0.0, _, Divide -> Just $ ExpressionLiteral 0.0
  _, _, _ -> Nothing

simplifyConstantLeafNodes :: Expression -> Expression -> BinaryOperation -> Maybe Expression
simplifyConstantLeafNodes simplifiedLeftExpression simplifiedRightExpression operation = case simplifiedLeftExpression, simplifiedRightExpression, operation of
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Plus -> Just $ ExpressionLiteral (leftValue + rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Minus -> Just $ ExpressionLiteral (leftValue - rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Times -> Just $ ExpressionLiteral (leftValue * rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Divide -> Just $ ExpressionLiteral (leftValue / rightValue)
  _, _, _ -> Nothing

simplifyCommutativeOperations :: Expression -> Expression -> BinaryOperation -> Maybe Expression
simplifyCommutativeOperations simplifiedLeftExpression simplifiedRightExpression operation = case simplifiedLeftExpression, simplifiedRightExpression, operation of
  ExpressionLiteral leftValue, ExpressionBinary Times (ExpressionLiteral nestedRightValue) nestedRightExpression, Times -> Just $ ExpressionBinary Times (ExpressionLiteral (leftValue * nestedRightValue)) nestedRightExpression
  ExpressionLiteral leftValue, ExpressionBinary Times nestedRightExpression (ExpressionLiteral nestedRightValue), Times -> Just $ ExpressionBinary Times (ExpressionLiteral (leftValue * nestedRightValue)) nestedRightExpression
  ExpressionBinary Times nestedLeftExpression (ExpressionLiteral nestedLeftValue), ExpressionLiteral rightValue, Times -> Just $ ExpressionBinary Times nestedLeftExpression (ExpressionLiteral (rightValue * nestedLeftValue))
  ExpressionBinary Times (ExpressionLiteral nestedLeftValue) nestedLeftExpression, ExpressionLiteral rightValue, Times -> Just $ ExpressionBinary Times nestedLeftExpression (ExpressionLiteral (rightValue * nestedLeftValue))
  _, _, _ -> Nothing
