module Expression.Differentiator (differentiate, secondDifferentiate) where

import Prelude

import Expression.SubExpression (removeSubExpressions)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))
import IntervalArith.Misc (Rational)

secondDifferentiate :: Expression -> Expression
secondDifferentiate = differentiate <<< differentiate

differentiate :: Expression -> Expression
differentiate = case _ of
  ExpressionLiteral _ -> ExpressionLiteral zero
  ExpressionVariable _ -> ExpressionLiteral one
  ExpressionBinary operation leftExpression rightExpression -> differentiateBinaryOperation operation leftExpression rightExpression
  ExpressionUnary operation expression -> differentiateUnaryOperation operation expression
  ExpressionLet name expression parentExpression -> differentiate $ removeSubExpressions $ ExpressionLet name expression parentExpression

differentiateBinaryOperation :: BinaryOperation -> Expression -> Expression -> Expression
differentiateBinaryOperation (Plus) leftExpression rightExpression = ExpressionBinary Plus (differentiate leftExpression) (differentiate rightExpression)

differentiateBinaryOperation (Minus) leftExpression rightExpression = ExpressionBinary Minus (differentiate leftExpression) (differentiate rightExpression)

differentiateBinaryOperation (Times) leftExpression rightExpression = ExpressionBinary Plus (ExpressionBinary Times u v') (ExpressionBinary Times u' v)
  -- Product rule
  where
  u = leftExpression

  v = rightExpression

  u' = differentiate u

  v' = differentiate v

differentiateBinaryOperation (Divide) topExpression bottomExpression = ExpressionBinary Divide (ExpressionBinary Minus (ExpressionBinary Times f' g) (ExpressionBinary Times f g')) (ExpressionBinary Power g (ExpressionLiteral two))
  -- Quotient rule
  where
  f = topExpression

  g = bottomExpression

  f' = differentiate f

  g' = differentiate g

differentiateBinaryOperation (Power) (ExpressionVariable "e") (ExpressionLiteral _) = ExpressionLiteral zero

differentiateBinaryOperation (Power) (ExpressionVariable "e") exponentExpression = ExpressionBinary Times (differentiate exponentExpression) (ExpressionUnary Exp exponentExpression)

differentiateBinaryOperation (Power) baseExpression (ExpressionLiteral value) = ExpressionBinary Times (ExpressionLiteral value) (ExpressionBinary Power baseExpression (ExpressionLiteral (value - one)))

differentiateBinaryOperation (Power) baseExpression exponentExpression = ExpressionBinary Times k (ExpressionBinary Plus j l)
  -- (g^f)' = g^(f-1) * ((f*g')+(g*f'*log(g)))
  -- (g^f)' = k * (j + l)
  -- source: https://www.wolframalpha.com/input/?i=(f%5E(g))%27
  where
  f = exponentExpression

  g = baseExpression

  f' = differentiate f

  g' = differentiate g

  -- k = g^(f - 1)
  k = ExpressionBinary Power g (ExpressionBinary Minus f (ExpressionLiteral (-one)))

  -- j = f * g'
  j = ExpressionBinary Times f g'

  -- l = g * f' * log(g)
  l = ExpressionBinary Times (ExpressionBinary Times g f) (ExpressionUnary Log g)

differentiateUnaryOperation :: UnaryOperation -> Expression -> Expression
differentiateUnaryOperation (Neg) expression = ExpressionUnary Neg $ differentiate expression

differentiateUnaryOperation (Sqrt) expression = ExpressionBinary Divide f' (ExpressionBinary Times (ExpressionLiteral two) (ExpressionUnary Sqrt f))
  -- (sqrt(f))' = f'/(2*sqrt(f))
  where
  f = expression

  f' = differentiate f

differentiateUnaryOperation (Exp) (ExpressionLiteral _) = ExpressionLiteral zero

differentiateUnaryOperation (Exp) expression = ExpressionBinary Times (differentiate expression) (ExpressionUnary Exp expression)

differentiateUnaryOperation (Log) expression = ExpressionBinary Divide (differentiate expression) (expression)

differentiateUnaryOperation (Sine) expression = ExpressionBinary Times (differentiate expression) (ExpressionUnary Cosine expression)

differentiateUnaryOperation (Cosine) expression = ExpressionUnary Neg $ (ExpressionBinary Times (differentiate expression) (ExpressionUnary Sine expression))

differentiateUnaryOperation (Tan) expression = ExpressionBinary Times f' (ExpressionBinary Plus (ExpressionLiteral one) k)
  -- tan(f)' = f' * (1 + tan^2(f)) 
  -- tan(f)' = f' * (1 + k)
  where
  f = expression

  f' = differentiate f

  -- k = tan^2(f)
  k = ExpressionBinary Power (ExpressionUnary Tan f) (ExpressionLiteral two)

two :: Rational
two = one + one