module Expression.Differentiator (differentiate, secondDifferentiate) where

import Prelude

import Expression.Simplifier (simplify)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))

secondDifferentiate :: Expression -> Expression
secondDifferentiate = simplify <<< differentiateExpression <<< differentiateExpression

differentiate :: Expression -> Expression
differentiate = simplify <<< differentiateExpression

differentiateExpression :: Expression -> Expression
differentiateExpression = case _ of
  ExpressionLiteral _ -> ExpressionLiteral 0.0
  ExpressionVariable _ -> ExpressionLiteral 1.0
  ExpressionBinary operation leftExpression rightExpression -> differentiateBinaryOperation operation leftExpression rightExpression
  ExpressionUnary operation expression -> differentiateUnaryOperation operation expression

differentiateBinaryOperation :: BinaryOperation -> Expression -> Expression -> Expression
differentiateBinaryOperation (Plus) leftExpression rightExpression = ExpressionBinary Plus (differentiateExpression leftExpression) (differentiateExpression rightExpression)

differentiateBinaryOperation (Minus) leftExpression rightExpression = ExpressionBinary Plus (differentiateExpression leftExpression) (differentiateExpression rightExpression)

differentiateBinaryOperation (Times) leftExpression rightExpression = ExpressionBinary Plus (ExpressionBinary Times u v') (ExpressionBinary Times u' v)
  -- Product rule
  where
  u = leftExpression

  v = rightExpression

  u' = differentiateExpression u

  v' = differentiateExpression v

differentiateBinaryOperation (Divide) topExpression bottomExpression = ExpressionBinary Divide (ExpressionBinary Minus (ExpressionBinary Times f' g) (ExpressionBinary Times f g')) (ExpressionBinary Power g (ExpressionLiteral 2.0))
  -- Quotient rule
  where
  f = topExpression

  g = bottomExpression

  f' = differentiateExpression f

  g' = differentiateExpression g

differentiateBinaryOperation (Power) mantisaExpression (ExpressionLiteral value) = ExpressionBinary Times (ExpressionLiteral value) (ExpressionBinary Power mantisaExpression (ExpressionLiteral (value - 1.0)))

differentiateBinaryOperation (Power) mantisaExpression exponentExpression = ExpressionBinary Times k (ExpressionBinary Plus j l)
  -- (g^f)' = g^(f-1) * ((f*g')+(g*f'*log(g)))
  -- (g^f)' = k * (j + l)
  -- source: https://www.wolframalpha.com/input/?i=(f%5E(g))%27
  where
  f = exponentExpression

  g = mantisaExpression

  f' = differentiateExpression f

  g' = differentiateExpression g

  -- k = g^(f - 1)
  k = ExpressionBinary Power g (ExpressionBinary Minus f (ExpressionLiteral (-1.0)))

  -- j = f * g'
  j = ExpressionBinary Times f g'

  -- l = g * f' * log(g)
  l = ExpressionBinary Times (ExpressionBinary Times g f) (ExpressionUnary Log g)

differentiateUnaryOperation :: UnaryOperation -> Expression -> Expression
differentiateUnaryOperation (Neg) expression = ExpressionUnary Neg $ differentiateExpression expression

differentiateUnaryOperation (Sqrt) expression = ExpressionBinary Divide f' (ExpressionBinary Times (ExpressionLiteral 2.0) (ExpressionUnary Sqrt f))
  -- (sqrt(f))' = f'/(2*sqrt(f))
  where
  f = expression

  f' = differentiateExpression f

differentiateUnaryOperation (Exp) expression = ExpressionBinary Times (differentiateExpression expression) (ExpressionUnary Exp expression)

differentiateUnaryOperation (Log) expression = ExpressionBinary Divide (differentiateExpression expression) (expression)

differentiateUnaryOperation (Sine) expression = ExpressionBinary Times (ExpressionUnary Cosine expression) (differentiateExpression expression)

differentiateUnaryOperation (Cosine) expression = ExpressionUnary Neg $ (ExpressionBinary Times (ExpressionUnary Sine expression) (differentiateExpression expression))

differentiateUnaryOperation (Tan) expression = ExpressionBinary Divide (ExpressionUnary Sine expression) (ExpressionUnary Cosine expression)
