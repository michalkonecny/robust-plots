module Expression.Simplifier where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ratio (denominator, numerator)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))
import IntervalArith.Misc (big, toRational)

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
trimTimesNodes (ExpressionLiteral leftValue) (ExpressionLiteral rightValue) = 
  if (big 0) == leftNumerator 
    then Just $ ExpressionLiteral (toRational 0)
    else if (big 0) == rightNumerator
      then Just $ ExpressionLiteral (toRational 0)
      else Nothing
  where
    leftNumerator = numerator leftValue
    rightNumerator = numerator rightValue
trimTimesNodes (ExpressionLiteral leftValue) rightExpression  = 
  if leftNumerator == leftDenominator
    then Just $ rightExpression
    else Nothing
  where
    leftNumerator = numerator leftValue
    leftDenominator = denominator leftValue
trimTimesNodes leftExpression (ExpressionLiteral rightValue) = 
  if rightNumerator == rightDenominator
    then Just $ leftExpression
    else Nothing
  where
    rightNumerator = numerator rightValue
    rightDenominator = denominator rightValue
trimTimesNodes _ _ = Nothing

trimPlusNodes :: Expression -> Expression -> Maybe Expression
trimPlusNodes (ExpressionLiteral leftValue) rightExpression  = 
  if (big 0) == leftNumerator 
    then Just $ rightExpression
    else Nothing
  where
    leftNumerator = numerator leftValue
trimPlusNodes leftExpression (ExpressionLiteral rightValue) = 
  if (big 0) == rightNumerator
    then Just $ leftExpression
    else Nothing
  where
    rightNumerator = numerator rightValue
trimPlusNodes _ _ = Nothing 

trimMinusNodes :: Expression -> Expression -> Maybe Expression
trimMinusNodes (ExpressionLiteral leftValue) rightExpression  = 
  if (big 0) == leftNumerator 
    then Just $ ExpressionUnary Neg rightExpression
    else Nothing
  where
    leftNumerator = numerator leftValue
trimMinusNodes leftExpression (ExpressionLiteral rightValue) = 
  if (big 0) == rightNumerator
    then Just $ leftExpression
    else Nothing
  where
    rightNumerator = numerator rightValue
trimMinusNodes _ _ = Nothing 

trimDivideNodes :: Expression -> Expression -> Maybe Expression
trimDivideNodes (ExpressionLiteral leftValue) rightExpression  = 
  if (big 0) == leftNumerator 
    then Just $ ExpressionLiteral $ toRational 0
    else Nothing
  where
    leftNumerator = numerator leftValue
trimDivideNodes _ _ = Nothing 

trimPowerNodes :: Expression -> Expression -> Maybe Expression
trimPowerNodes leftExpression (ExpressionLiteral rightValue) = 
  if rightNumerator == (big 0)
    then Just $ ExpressionLiteral $ toRational 1
    else if rightNumerator == rightDenominator
        then Just $ leftExpression
        else Nothing
  where
    rightNumerator = numerator rightValue
    rightDenominator = denominator rightValue
trimPowerNodes _ _ = Nothing 

trimSimpleNodes :: Expression -> Expression -> BinaryOperation -> Maybe Expression
trimSimpleNodes leftExpression rightExpresson Times = trimTimesNodes leftExpression rightExpresson
trimSimpleNodes leftExpression rightExpresson Plus = trimPlusNodes leftExpression rightExpresson
trimSimpleNodes leftExpression rightExpresson Minus = trimMinusNodes leftExpression rightExpresson
trimSimpleNodes leftExpression rightExpresson Power = trimPowerNodes leftExpression rightExpresson
trimSimpleNodes _ _ _ = Nothing

trimConstantLeafNodes :: Expression -> Expression -> BinaryOperation -> Maybe Expression
trimConstantLeafNodes simplifiedLeftExpression simplifiedRightExpression operation = case simplifiedLeftExpression, simplifiedRightExpression, operation of
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Plus -> Just $ ExpressionLiteral (leftValue + rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Minus -> Just $ ExpressionLiteral (leftValue - rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Times -> Just $ ExpressionLiteral (leftValue * rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Divide -> Just $ ExpressionLiteral (leftValue / rightValue)
  _, _, _ -> Nothing

trimTimesOperations :: Expression -> Expression -> BinaryOperation -> Maybe Expression
trimTimesOperations simplifiedLeftExpression simplifiedRightExpression operation = case simplifiedLeftExpression, simplifiedRightExpression, operation of
  ExpressionLiteral leftValue, ExpressionBinary Times (ExpressionLiteral nestedRightValue) nestedRightExpression, Times -> Just $ ExpressionBinary Times (ExpressionLiteral (leftValue * nestedRightValue)) nestedRightExpression
  ExpressionLiteral leftValue, ExpressionBinary Times nestedRightExpression (ExpressionLiteral nestedRightValue), Times -> Just $ ExpressionBinary Times (ExpressionLiteral (leftValue * nestedRightValue)) nestedRightExpression
  ExpressionBinary Times nestedLeftExpression (ExpressionLiteral nestedLeftValue), ExpressionLiteral rightValue, Times -> Just $ ExpressionBinary Times nestedLeftExpression (ExpressionLiteral (rightValue * nestedLeftValue))
  ExpressionBinary Times (ExpressionLiteral nestedLeftValue) nestedLeftExpression, ExpressionLiteral rightValue, Times -> Just $ ExpressionBinary Times nestedLeftExpression (ExpressionLiteral (rightValue * nestedLeftValue))
  _, _, _ -> Nothing

trimPlusOperations :: Expression -> Expression -> BinaryOperation -> Maybe Expression
trimPlusOperations simplifiedLeftExpression simplifiedRightExpression operation = case simplifiedLeftExpression, simplifiedRightExpression, operation of
  ExpressionLiteral leftValue, ExpressionBinary Plus (ExpressionLiteral nestedRightValue) nestedRightExpression, Plus -> Just $ ExpressionBinary Plus (ExpressionLiteral (leftValue + nestedRightValue)) nestedRightExpression
  ExpressionLiteral leftValue, ExpressionBinary Plus nestedRightExpression (ExpressionLiteral nestedRightValue), Plus -> Just $ ExpressionBinary Plus (ExpressionLiteral (leftValue + nestedRightValue)) nestedRightExpression
  ExpressionBinary Plus nestedLeftExpression (ExpressionLiteral nestedLeftValue), ExpressionLiteral rightValue, Plus -> Just $ ExpressionBinary Plus nestedLeftExpression (ExpressionLiteral (rightValue + nestedLeftValue))
  ExpressionBinary Plus (ExpressionLiteral nestedLeftValue) nestedLeftExpression, ExpressionLiteral rightValue, Plus -> Just $ ExpressionBinary Plus nestedLeftExpression (ExpressionLiteral (rightValue + nestedLeftValue))
  _, _, _ -> Nothing
