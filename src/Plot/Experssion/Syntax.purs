module Plot.Expression.Syntax
  ( Expression(..)
  , Constant(..)
  , UnaryOperation(..)
  , BinaryOperation(..)
  , VariableName
  ) where

import Prelude
import Data.Int (round, toNumber)

data Constant
  = Pi
  | E

derive instance constantEq :: Eq Constant

derive instance constantOrd :: Ord Constant

instance constantShow :: Show Constant where
  show Pi = "pi"
  show E = "e"

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
  show Exp = "exp"
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

type VariableName = String

data Expression
  = ExpressionVariable VariableName
  | ExpressionLiteral Number
  | ExpressionConstant Constant
  | ExpressionUnary UnaryOperation Expression
  | ExpressionBinary BinaryOperation Expression Expression

derive instance expressionEq :: Eq Expression

derive instance expressionOrd :: Ord Expression

instance expressionShow :: Show Expression where
  show (ExpressionVariable name) = name
  show (ExpressionLiteral value) = showLiteral value
  show (ExpressionConstant constant) = show constant
  show (ExpressionUnary unaryOperation expression) = (show unaryOperation) <> (showNestedExpression expression)
  show (ExpressionBinary binaryOperation leftExpression rightExpression) = (showNestedExpression leftExpression) <> (show binaryOperation) <> (showNestedExpression rightExpression)

showLiteral :: Number -> String
showLiteral value =
  if isInteger then
    show integerValue
  else
    show value
  where
  integerValue = round value

  isInteger = value == toNumber integerValue

showNestedExpression :: Expression -> String
showNestedExpression (ExpressionVariable name) = show $ ExpressionVariable name

showNestedExpression (ExpressionLiteral value) = show $ ExpressionLiteral value

showNestedExpression expression = "(" <> (show expression) <> ")"
