module Expression.SubExpression where

import Prelude
import Data.Array (fromFoldable, mapWithIndex, sortBy, uncons)
import Data.Maybe (Maybe(..))
import Data.Set (Set, empty, insert)
import Data.Tuple (Tuple(..))
import Expression.Syntax (Expression(..), VariableName)
import Expression.VariableMap (VariableMap, lookup)

removeSubExpressions :: Expression -> Expression
removeSubExpressions = removeSubExpressionsWithMap []
  where
  removeSubExpressionsWithMap :: (VariableMap Expression) -> Expression -> Expression
  removeSubExpressionsWithMap variableMap = case _ of
    ExpressionVariable name -> case lookup variableMap name of
      Just value -> value
      _ -> ExpressionVariable name
    ExpressionLet name expression parentExpression -> removeSubExpressionsWithMap (variableMap <> [ (Tuple name expression) ]) parentExpression
    expression -> expression

joinCommonSubExpressions :: Expression -> Expression
joinCommonSubExpressions expression = addSubExpressionsUntilConvergence variableDefinitions expression
  where
  commonSubExpressionsOrderedByOccurances = sortByOccurances $ fromFoldable $ splitSubExpressions expression

  variableDefinitions = mapWithIndex assignVariableName commonSubExpressionsOrderedByOccurances

addSubExpressionsUntilConvergence :: Array VariableExpression -> Expression -> Expression
addSubExpressionsUntilConvergence variableDefinitions expression = case uncons variableDefinitions of
  Nothing -> expression
  Just { head, tail } -> ExpressionLet head.name head.expression result
    where
    addedHeadVariable = substitute head expression

    newVariableDefinitions = map (\target -> target { expression = substitute head target.expression }) tail

    result = addSubExpressionsUntilConvergence newVariableDefinitions addedHeadVariable

substitute :: VariableExpression -> Expression -> Expression
substitute target expression =
  if expression == target.expression then
    ExpressionVariable target.name
  else case expression of
    ExpressionUnary unaryOperation nestedExpression -> ExpressionUnary unaryOperation $ substitute target nestedExpression
    ExpressionBinary binaryOperation leftExpression rightExpression -> ExpressionBinary binaryOperation (substitute target leftExpression) (substitute target rightExpression)
    ExpressionLet name variableExpression parentExpression -> ExpressionLet name variableExpression (substitute target parentExpression)
    -- If the expression is not the target and not a binary or unary operation then it is already clean.
    _ -> expression

assignVariableName :: Int -> Expression -> VariableExpression
assignVariableName index expression = { expression, name: "$v" <> (show (index + 1)) }

splitSubExpressions :: Expression -> Set Expression
splitSubExpressions = splitSubExpressionsWithMap empty
  where
  splitSubExpressionsWithMap :: Set Expression -> Expression -> Set Expression
  splitSubExpressionsWithMap counter = case _ of
    ExpressionUnary unaryOperation expression -> splitSubExpressionsWithMap newCounter expression
      where
      newCounter = insert (ExpressionUnary unaryOperation expression) counter
    ExpressionBinary binaryOperation leftExpression rightExpression -> rightCounter
      where
      newCounter = insert (ExpressionBinary binaryOperation leftExpression rightExpression) counter

      leftCounter = splitSubExpressionsWithMap newCounter leftExpression

      rightCounter = splitSubExpressionsWithMap leftCounter rightExpression
    -- We dont want to count literals or variables
    expression -> counter

type VariableExpression
  = { expression :: Expression, name :: VariableName }

sortByOccurances :: Array Expression -> Array Expression
sortByOccurances = sortBy compareOccurances

compareOccurances :: Expression -> Expression -> Ordering
compareOccurances a b = if isASubExpressionOf a b then GT else EQ

isASubExpressionOf :: Expression -> Expression -> Boolean
isASubExpressionOf target expression =
  expression == target
    || case expression of
        ExpressionUnary _ nestedExpression -> isASubExpressionOf target nestedExpression
        ExpressionBinary _ leftExpression rightExpression -> (isASubExpressionOf target leftExpression) || (isASubExpressionOf target rightExpression)
        -- We dont want to count literals or variables
        _ -> false
