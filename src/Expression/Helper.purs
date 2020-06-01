module Expression.Helper where

import Prelude
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))

foldConstants :: Expression -> Expression
foldConstants (ExpressionUnary operation expression) = case foldConstants expression, operation of
  ExpressionLiteral value, Neg -> ExpressionLiteral (-value)
  foldedExpression, _ -> ExpressionUnary operation foldedExpression

foldConstants (ExpressionBinary Power leftExpression (ExpressionLiteral 1.0)) = foldConstants leftExpression

foldConstants (ExpressionBinary operation leftExpression rightExpression) = case foldConstants leftExpression, foldConstants rightExpression, operation of
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Plus -> ExpressionLiteral (leftValue + rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Minus -> ExpressionLiteral (leftValue - rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Times -> ExpressionLiteral (leftValue * rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Divide -> ExpressionLiteral (leftValue / rightValue)
  foldedLeftExpression, foldedRightExpression, _ -> ExpressionBinary operation foldedLeftExpression foldedRightExpression

foldConstants expression = expression
