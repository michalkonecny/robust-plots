module Expression.SubExpression where

import Prelude

import Data.Array (delete, filter, find, mapWithIndex, reverse, sortBy, uncons)
import Data.Maybe (Maybe(..))
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
  commonSubExpressionsOrderedByOccurances = reverse $ sortByOccurances $ filterSingleOccurances $ countOccurances expression
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

assignVariableName :: Int -> SubExpressionCount -> VariableExpression
assignVariableName index { expression } = { expression, name: "$v" <> (show (index + 1)) }

countOccurances :: Expression -> SubExpressionCounter
countOccurances = countOccurancesWithMap []
  where
  countOccurancesWithMap :: SubExpressionCounter -> Expression -> SubExpressionCounter
  countOccurancesWithMap counter = case _ of
    ExpressionUnary unaryOperation expression -> countOccurancesWithMap newCounter expression
      where
      newCounter = addExpressionToCounter counter (ExpressionUnary unaryOperation expression)
    ExpressionBinary binaryOperation leftExpression rightExpression -> rightCounter
      where
      newCounter = addExpressionToCounter counter (ExpressionBinary binaryOperation leftExpression rightExpression)

      leftCounter = countOccurancesWithMap newCounter leftExpression

      rightCounter = countOccurancesWithMap leftCounter rightExpression
    -- We dont want to count literals or variables
    expression -> counter

type VariableExpression
  = { expression :: Expression, name :: VariableName }

type SubExpressionCount
  = { expression :: Expression, occurances :: Int }

type SubExpressionCounter
  = Array SubExpressionCount

filterSingleOccurances :: SubExpressionCounter -> SubExpressionCounter
filterSingleOccurances = filter (\count -> count.occurances > 1)

sortByOccurances :: SubExpressionCounter -> SubExpressionCounter
sortByOccurances = sortBy compareOccurances

compareOccurances :: SubExpressionCount -> SubExpressionCount -> Ordering
compareOccurances a b = compare (a.occurances) (b.occurances)

addExpressionToCounter :: SubExpressionCounter -> Expression -> SubExpressionCounter
addExpressionToCounter counter expression = case lookupCount counter expression of
  Just count -> (delete count counter) <> [ ({ expression, occurances: count.occurances + 1 }) ]
  Nothing -> counter <> [ { expression, occurances: 1 } ]

lookupCount :: SubExpressionCounter -> Expression -> Maybe SubExpressionCount
lookupCount counter expression = find (isThisExpression expression) counter

isThisExpression :: Expression -> SubExpressionCount -> Boolean
isThisExpression target { expression, occurances } = expression == target
