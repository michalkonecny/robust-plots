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
  show (ExpressionUnary unaryOperation expression) = (show unaryOperation) <> (showNestedExpression expression)
  show (ExpressionBinary binaryOperation leftExpression rightExpression) = (showNestedExpression leftExpression) <> (show binaryOperation) <> (showNestedExpression rightExpression)
  show (ExpressionLet name expression parentExpression) = "let " <> (show name) <> " = " <> (show expression) <> " in " <> (show parentExpression)

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

showNestedExpression :: Expression -> String
showNestedExpression (ExpressionVariable name) = show $ ExpressionVariable name

showNestedExpression (ExpressionLiteral value) = show $ ExpressionLiteral value

showNestedExpression expression = "(" <> (show expression) <> ")"
