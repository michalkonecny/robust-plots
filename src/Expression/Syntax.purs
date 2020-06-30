module Expression.Syntax
  ( Expression(..)
  , UnaryOperation(..)
  , BinaryOperation(..)
  , VariableName
  ) where

import Prelude
import Data.Int (round, toNumber)
import IntervalArith.Misc (Rational, rationalToNumber)

data UnaryOperation
  = Neg
  | Sqrt
  | Exp
  | Log
  | Sine
  | Cosine
  | Tan

derive instance unaryOperationEq :: Eq UnaryOperation

derive instance unaryOperationOrd :: Ord UnaryOperation

instance unaryOperationShow :: Show UnaryOperation where
  show Neg = "-"
  show Sqrt = "sqrt"
  show Exp = "e^"
  show Log = "log"
  show Sine = "sin"
  show Cosine = "cos"
  show Tan = "tan"

data BinaryOperation
  = Plus
  | Minus
  | Times
  | Divide
  | Power

derive instance binaryOperationEq :: Eq BinaryOperation

derive instance binaryOperationOrd :: Ord BinaryOperation

instance binaryOperationShow :: Show BinaryOperation where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divide = "/"
  show Power = "^"

type VariableName
  = String

data Expression
  = ExpressionVariable VariableName
  | ExpressionLiteral Rational
  | ExpressionUnary UnaryOperation Expression
  | ExpressionBinary BinaryOperation Expression Expression
  | ExpressionLet VariableName Expression Expression

derive instance expressionEq :: Eq Expression

derive instance expressionOrd :: Ord Expression

instance expressionShow :: Show Expression where
  show (ExpressionVariable name) = name
  show (ExpressionLiteral value) = showLiteral value
  show (ExpressionUnary unaryOperation expression) = showUnaryExpression unaryOperation expression
  show (ExpressionBinary binaryOperation leftExpression rightExpression) = (showNestedBinaryExpression leftExpression) <> (show binaryOperation) <> (showNestedBinaryExpression rightExpression)
  show (ExpressionLet name expression parentExpression) = "let " <> name <> " = " <> (show expression) <> " in " <> (show parentExpression)

showLiteral :: Rational -> String
showLiteral value =
  if isInteger then
    show integerValue
  else
    show numberValue
  where
  numberValue = rationalToNumber value

  integerValue = round $ numberValue

  isInteger = numberValue == toNumber integerValue

showUnaryExpression :: UnaryOperation -> Expression -> String
showUnaryExpression Neg expression@(ExpressionVariable name) = (show Neg) <> (show expression)
showUnaryExpression Neg expression@(ExpressionLiteral value) = (show Neg) <> (show expression)
showUnaryExpression Exp expression@(ExpressionVariable name) = (show Exp) <> (show expression)
showUnaryExpression Exp expression@(ExpressionLiteral value) = (show Exp) <> (show expression)
showUnaryExpression unaryOperation expression = (show unaryOperation) <> "(" <> (show expression) <> ")"

showNestedBinaryExpression :: Expression -> String
showNestedBinaryExpression (ExpressionVariable name) = show $ ExpressionVariable name

showNestedBinaryExpression (ExpressionLiteral value) = show $ ExpressionLiteral value

showNestedBinaryExpression expression = "(" <> (show expression) <> ")"
