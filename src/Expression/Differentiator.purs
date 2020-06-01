module Expression.Differentiator where

import Prelude
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))

secondDifferentiate :: Expression -> Expression
secondDifferentiate = differentiate <<< differentiate

differentiate :: Expression -> Expression
differentiate = case _ of
  ExpressionLiteral _ -> ExpressionLiteral 0.0
  ExpressionVariable _ -> ExpressionLiteral 1.0
  ExpressionBinary operation leftExpression rightExpression -> differentiateBinaryOperation operation leftExpression rightExpression
  ExpressionUnary operation expression -> differentiateUnaryOperation operation expression

differentiateBinaryOperation :: BinaryOperation -> Expression -> Expression -> Expression
differentiateBinaryOperation (Plus) leftExpression rightExpression = ExpressionBinary Plus (differentiate leftExpression) (differentiate rightExpression)

differentiateBinaryOperation (Minus) leftExpression rightExpression = ExpressionBinary Plus (differentiate leftExpression) (differentiate rightExpression)

differentiateBinaryOperation (Times) leftExpression rightExpression = ExpressionBinary Plus (ExpressionBinary Times u v') (ExpressionBinary Times u' v)
  -- Product rule
  where
  u = leftExpression

  v = rightExpression

  u' = differentiate u

  v' = differentiate v

differentiateBinaryOperation (Divide) topExpression bottomExpression = ExpressionBinary Divide (ExpressionBinary Minus (ExpressionBinary Times f' g) (ExpressionBinary Times f g')) (ExpressionBinary Power g (ExpressionLiteral 2.0))
  -- Quotient rule
  where
  f = topExpression

  g = bottomExpression

  f' = differentiate f

  g' = differentiate g

differentiateBinaryOperation (Power) mantisaExpression (ExpressionLiteral value) = ExpressionBinary Times (ExpressionLiteral value) (ExpressionBinary Power mantisaExpression (ExpressionLiteral (value - 1.0)))

differentiateBinaryOperation (Power) mantisaExpression exponentExpression = ExpressionBinary Times k (ExpressionBinary Plus j l)
  -- (g^f)' = g^(f-1) * ((f*g')+(g*f'*log(g)))
  -- (g^f)' = k * (j + l)
  -- source: https://www.wolframalpha.com/input/?i=(f%5E(g))%27
  where
  f = exponentExpression

  g = mantisaExpression

  f' = differentiate f

  g' = differentiate g

  -- k = g^(f - 1)
  k = ExpressionBinary Power g (ExpressionBinary Minus f (ExpressionLiteral (-1.0)))

  -- j = f * g'
  j = ExpressionBinary Times f g'

  -- l = g * f' * log(g)
  l = ExpressionBinary Times (ExpressionBinary Times g f) (ExpressionUnary Log g)

differentiateUnaryOperation :: UnaryOperation -> Expression -> Expression
differentiateUnaryOperation (Neg) expression = ExpressionUnary Neg $ differentiate expression

differentiateUnaryOperation (Sqrt) expression = ExpressionBinary Divide f' (ExpressionBinary Times (ExpressionLiteral 2.0) (ExpressionUnary Sqrt f))
  -- (sqrt(f))' = f'/(2*sqrt(f))
  where
  f = expression

  f' = differentiate f

differentiateUnaryOperation (Exp) expression = ExpressionBinary Times (differentiate expression) (ExpressionUnary Exp expression)

differentiateUnaryOperation (Log) expression = ExpressionBinary Divide (differentiate expression) (expression)

differentiateUnaryOperation (Sine) expression = ExpressionBinary Times (ExpressionUnary Cosine expression) (differentiate expression)

differentiateUnaryOperation (Cosine) expression = ExpressionUnary Neg $ (ExpressionBinary Times (ExpressionUnary Sine expression) (differentiate expression))

differentiateUnaryOperation (Tan) expression = ExpressionBinary Divide (ExpressionUnary Sine expression) (ExpressionUnary Cosine expression)
