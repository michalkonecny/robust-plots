module Expression.Differentiator (differentiate, secondDifferentiate) where

import Prelude

import Expression.SubExpression (removeSubExpressions)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..), VariableName)
import IntervalArith.Misc (Rational)

secondDifferentiate :: VariableName -> Expression -> Expression
secondDifferentiate x = differentiate x <<< differentiate x

differentiate :: VariableName -> Expression -> Expression
differentiate x = case _ of
  ExpressionLiteral _ -> ExpressionLiteral zero
  ExpressionVariable varName | varName == x -> ExpressionLiteral one
  ExpressionVariable _ -> ExpressionLiteral zero
  ExpressionBinary operation leftExpression rightExpression -> differentiateBinaryOperation x operation leftExpression rightExpression
  ExpressionUnary operation expression -> differentiateUnaryOperation x operation expression
  ExpressionLet name expression parentExpression -> differentiate x $ removeSubExpressions $ ExpressionLet name expression parentExpression

differentiateBinaryOperation :: VariableName -> BinaryOperation -> Expression -> Expression -> Expression
differentiateBinaryOperation x (Plus) leftExpression rightExpression = ExpressionBinary Plus (differentiate x leftExpression) (differentiate x rightExpression)

differentiateBinaryOperation x (Minus) leftExpression rightExpression = ExpressionBinary Minus (differentiate x leftExpression) (differentiate x rightExpression)

differentiateBinaryOperation x (Times) leftExpression rightExpression = ExpressionBinary Plus (ExpressionBinary Times u v') (ExpressionBinary Times u' v)
  -- Product rule
  where
  u = leftExpression

  v = rightExpression

  u' = differentiate x u

  v' = differentiate x v

differentiateBinaryOperation x (Divide) topExpression bottomExpression = ExpressionBinary Divide (ExpressionBinary Minus (ExpressionBinary Times f' g) (ExpressionBinary Times f g')) (ExpressionBinary Power g (ExpressionLiteral two))
  -- Quotient rule
  where
  f = topExpression

  g = bottomExpression

  f' = differentiate x f

  g' = differentiate x g

differentiateBinaryOperation x (Power) (ExpressionVariable "e") (ExpressionLiteral _) = ExpressionLiteral zero

differentiateBinaryOperation x (Power) (ExpressionVariable "e") exponentExpression = ExpressionBinary Times (differentiate x exponentExpression) (ExpressionUnary Exp exponentExpression)

differentiateBinaryOperation x (Power) baseExpression (ExpressionLiteral value) = ExpressionBinary Times (ExpressionLiteral value) (ExpressionBinary Power baseExpression (ExpressionLiteral (value - one)))

differentiateBinaryOperation x (Power) baseExpression exponentExpression = ExpressionBinary Times k (ExpressionBinary Plus j l)
  -- (g^f)' = g^(f-1) * ((f*g')+(g*f'*log(g)))
  -- (g^f)' = k * (j + l)
  -- source: https://www.wolframalpha.com/input/?i=(f%5E(g))%27
  where
  f = exponentExpression

  g = baseExpression

  f' = differentiate x f

  g' = differentiate x g

  -- k = g^(f - 1)
  k = ExpressionBinary Power g (ExpressionBinary Minus f (ExpressionLiteral (-one)))

  -- j = f * g'
  j = ExpressionBinary Times f g'

  -- l = g * f' * log(g)
  l = ExpressionBinary Times (ExpressionBinary Times g f) (ExpressionUnary Log g)

differentiateUnaryOperation :: VariableName -> UnaryOperation -> Expression -> Expression
differentiateUnaryOperation x (Neg) expression = ExpressionUnary Neg $ differentiate x expression

differentiateUnaryOperation x (Sqrt) expression = ExpressionBinary Divide f' (ExpressionBinary Times (ExpressionLiteral two) (ExpressionUnary Sqrt f))
  -- (sqrt(f))' = f'/(2*sqrt(f))
  where
  f = expression

  f' = differentiate x f

differentiateUnaryOperation x (Exp) (ExpressionLiteral _) = ExpressionLiteral zero

differentiateUnaryOperation x (Exp) expression = ExpressionBinary Times (differentiate x expression) (ExpressionUnary Exp expression)

differentiateUnaryOperation x (Log) expression = ExpressionBinary Divide (differentiate x expression) (expression)

differentiateUnaryOperation x (Sine) expression = ExpressionBinary Times (differentiate x expression) (ExpressionUnary Cosine expression)

differentiateUnaryOperation x (Cosine) expression = ExpressionUnary Neg $ (ExpressionBinary Times (differentiate x expression) (ExpressionUnary Sine expression))

differentiateUnaryOperation x (Tan) expression = ExpressionBinary Times f' (ExpressionBinary Plus (ExpressionLiteral one) k)
  -- tan(f)' = f' * (1 + tan^2(f)) 
  -- tan(f)' = f' * (1 + k)
  where
  f = expression

  f' = differentiate x f

  -- k = tan^2(f)
  k = ExpressionBinary Power (ExpressionUnary Tan f) (ExpressionLiteral two)

two :: Rational
two = one + one