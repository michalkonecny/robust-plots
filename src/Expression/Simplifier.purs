module Expression.Simplifier where

import Prelude
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))

simplify :: Expression -> Expression
simplify (ExpressionUnary operation expression) = case simplify expression, operation of
  ExpressionLiteral value, Neg -> ExpressionLiteral (-value)
  foldedExpression, _ -> ExpressionUnary operation foldedExpression

simplify (ExpressionBinary Power leftExpression (ExpressionLiteral 1.0)) = simplify leftExpression

simplify (ExpressionBinary operation leftExpression rightExpression) = case simplify leftExpression, simplify rightExpression, operation of
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Plus -> ExpressionLiteral (leftValue + rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Minus -> ExpressionLiteral (leftValue - rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Times -> ExpressionLiteral (leftValue * rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Divide -> ExpressionLiteral (leftValue / rightValue)
  foldedLeftExpression, foldedRightExpression, _ -> ExpressionBinary operation foldedLeftExpression foldedRightExpression

simplify expression = expression
