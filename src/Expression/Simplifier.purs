module Expression.Simplifier where

import Prelude

import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))

simplify :: Expression -> Expression
simplify (ExpressionUnary operation expression) = case simplify expression, operation of
  ExpressionLiteral value, Neg -> ExpressionLiteral (-value)
  simplifiedExpression, _ -> ExpressionUnary operation simplifiedExpression

simplify (ExpressionBinary Power leftExpression (ExpressionLiteral 1.0)) = simplify leftExpression

simplify (ExpressionBinary operation leftExpression rightExpression) = case simplify leftExpression, simplify rightExpression, operation of
  -- Trim zero leaf nodes
  ExpressionLiteral 0.0, simplifiedRightExpression, Times -> ExpressionLiteral 0.0
  simplifiedLeftExpression, ExpressionLiteral 0.0, Times -> ExpressionLiteral 0.0
  ExpressionLiteral 0.0, simplifiedRightExpression, Plus -> simplifiedRightExpression
  simplifiedLeftExpression, ExpressionLiteral 0.0, Plus -> simplifiedLeftExpression
  ExpressionLiteral 0.0, simplifiedRightExpression, Minus -> ExpressionUnary Neg simplifiedRightExpression
  simplifiedLeftExpression, ExpressionLiteral 0.0, Minus -> simplifiedLeftExpression
  ExpressionLiteral 0.0, simplifiedRightExpression, Divide -> ExpressionLiteral 0.0
  -- Simplify constant leaf nodes
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Plus -> ExpressionLiteral (leftValue + rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Minus -> ExpressionLiteral (leftValue - rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Times -> ExpressionLiteral (leftValue * rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Divide -> ExpressionLiteral (leftValue / rightValue)
  simplifiedLeftExpression, simplifiedRightExpression, _ -> ExpressionBinary operation simplifiedLeftExpression simplifiedRightExpression

simplify expression = expression
