module Expression.Simplifier where

import Prelude
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), fromMaybe)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))
import IntervalArith.Misc (branchRationalIsInt, toRational)

simplify :: Expression -> Expression
simplify (ExpressionUnary operation expression) = case simplify expression, operation of
  ExpressionLiteral value, Neg -> ExpressionLiteral (-value)
  simplifiedExpression, _ -> ExpressionUnary operation simplifiedExpression

simplify (ExpressionBinary Power (ExpressionVariable "e") rightExpression) = ExpressionUnary Exp $ simplify rightExpression

simplify (ExpressionBinary operation leftExpression rightExpression) =
  fromMaybe (ExpressionBinary operation simplifiedLeftExpression simplifiedRightExpression)
    $ trimSimpleNodes simplifiedLeftExpression simplifiedRightExpression operation
    <|> trimConstantLeafNodes simplifiedLeftExpression simplifiedRightExpression operation
    <|> trimTimesOperations simplifiedLeftExpression simplifiedRightExpression operation
    <|> trimPlusOperations simplifiedLeftExpression simplifiedRightExpression operation
  where
  simplifiedLeftExpression = simplify leftExpression

  simplifiedRightExpression = simplify rightExpression

simplify expression = expression

trimTimesNodes :: Expression -> Expression -> Maybe Expression
trimTimesNodes (ExpressionLiteral leftValue) rightExpression =
  if (toRational 1) == leftValue then
    Just $ rightExpression
  else
    if (toRational 0) == leftValue then
      Just $ ExpressionLiteral (toRational 0)
    else
      Nothing

trimTimesNodes leftExpression (ExpressionLiteral rightValue) =
  if (toRational 1) == rightValue then
    Just $ leftExpression
  else
    if (toRational 0) == rightValue then
      Just $ ExpressionLiteral (toRational 0)
    else
      Nothing

trimTimesNodes _ _ = Nothing

trimPlusNodes :: Expression -> Expression -> Maybe Expression
trimPlusNodes (ExpressionLiteral leftValue) rightExpression =
  if (toRational 0) == leftValue then
    Just $ rightExpression
  else
    Nothing

trimPlusNodes leftExpression (ExpressionLiteral rightValue) =
  if (toRational 0) == rightValue then
    Just $ leftExpression
  else
    Nothing

trimPlusNodes _ _ = Nothing

trimMinusNodes :: Expression -> Expression -> Maybe Expression
trimMinusNodes (ExpressionLiteral leftValue) rightExpression =
  if (toRational 0) == leftValue then
    Just $ ExpressionUnary Neg rightExpression
  else
    Nothing

trimMinusNodes leftExpression (ExpressionLiteral rightValue) =
  if (toRational 0) == rightValue then
    Just $ leftExpression
  else
    Nothing

trimMinusNodes _ _ = Nothing

trimDivideNodes :: Expression -> Expression -> Maybe Expression
trimDivideNodes (ExpressionLiteral leftValue) rightExpression =
  if (toRational 0) == leftValue then
    Just $ ExpressionLiteral $ toRational 0
  else
    Nothing

trimDivideNodes _ _ = Nothing

trimPowerNodes :: Expression -> Expression -> Maybe Expression
trimPowerNodes leftExpression (ExpressionLiteral rightValue)
  | (toRational 0) == rightValue = Just $ ExpressionLiteral $ toRational 1
  | (toRational 1) == rightValue = Just $ leftExpression
  | otherwise =
    branchRationalIsInt
      { isNotInt: \r -> Nothing
      , isInt: \i -> Just $ ExpressionIntPower leftExpression i
      }
      rightValue

trimPowerNodes _ _ = Nothing

trimSimpleNodes :: Expression -> Expression -> BinaryOperation -> Maybe Expression
trimSimpleNodes leftExpression rightExpresson Times = trimTimesNodes leftExpression rightExpresson

trimSimpleNodes leftExpression rightExpresson Plus = trimPlusNodes leftExpression rightExpresson

trimSimpleNodes leftExpression rightExpresson Minus = trimMinusNodes leftExpression rightExpresson

trimSimpleNodes leftExpression rightExpresson Divide = trimDivideNodes leftExpression rightExpresson

trimSimpleNodes leftExpression rightExpresson Power = trimPowerNodes leftExpression rightExpresson

trimSimpleNodes leftExpression rightExpresson _ = Nothing

trimConstantLeafNodes :: Expression -> Expression -> BinaryOperation -> Maybe Expression
trimConstantLeafNodes simplifiedLeftExpression simplifiedRightExpression operation = case simplifiedLeftExpression, simplifiedRightExpression, operation of
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Plus -> Just $ ExpressionLiteral (leftValue + rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Minus -> Just $ ExpressionLiteral (leftValue - rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Times -> Just $ ExpressionLiteral (leftValue * rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Divide -> Just $ ExpressionLiteral (leftValue / rightValue)
  _, _, _ -> Nothing

trimTimesOperations :: Expression -> Expression -> BinaryOperation -> Maybe Expression
trimTimesOperations simplifiedLeftExpression simplifiedRightExpression operation = case simplifiedLeftExpression, simplifiedRightExpression, operation of
  ExpressionLiteral leftValue, rightExpression, Times -> case rightExpression of
    ExpressionBinary Times (ExpressionLiteral nestedRightValue) nestedRightExpression -> Just $ ExpressionBinary Times (ExpressionLiteral (leftValue * nestedRightValue)) nestedRightExpression
    ExpressionBinary Times nestedRightExpression (ExpressionLiteral nestedRightValue) -> Just $ ExpressionBinary Times (ExpressionLiteral (leftValue * nestedRightValue)) nestedRightExpression
    _ -> Nothing
  leftExpression, ExpressionLiteral rightValue, Times -> case leftExpression of
    ExpressionBinary Times nestedLeftExpression (ExpressionLiteral nestedLeftValue) -> Just $ ExpressionBinary Times nestedLeftExpression (ExpressionLiteral (rightValue * nestedLeftValue))
    ExpressionBinary Times (ExpressionLiteral nestedLeftValue) nestedLeftExpression -> Just $ ExpressionBinary Times nestedLeftExpression (ExpressionLiteral (rightValue * nestedLeftValue))
    _ -> Nothing
  _, _, _ -> Nothing

trimPlusOperations :: Expression -> Expression -> BinaryOperation -> Maybe Expression
trimPlusOperations simplifiedLeftExpression simplifiedRightExpression operation = case simplifiedLeftExpression, simplifiedRightExpression, operation of
  ExpressionLiteral leftValue, rightExpression, Plus -> case rightExpression of
    ExpressionBinary Plus (ExpressionLiteral nestedRightValue) nestedRightExpression -> Just $ ExpressionBinary Plus (ExpressionLiteral (leftValue + nestedRightValue)) nestedRightExpression
    ExpressionBinary Plus nestedRightExpression (ExpressionLiteral nestedRightValue) -> Just $ ExpressionBinary Plus (ExpressionLiteral (leftValue + nestedRightValue)) nestedRightExpression
    _ -> Nothing
  leftExpression, ExpressionLiteral rightValue, Plus -> case leftExpression of
    ExpressionBinary Plus nestedLeftExpression (ExpressionLiteral nestedLeftValue) -> Just $ ExpressionBinary Plus nestedLeftExpression (ExpressionLiteral (rightValue + nestedLeftValue))
    ExpressionBinary Plus (ExpressionLiteral nestedLeftValue) nestedLeftExpression -> Just $ ExpressionBinary Plus nestedLeftExpression (ExpressionLiteral (rightValue + nestedLeftValue))
    _ -> Nothing
  _, _, _ -> Nothing
