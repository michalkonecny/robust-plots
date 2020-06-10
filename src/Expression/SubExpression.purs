module Expression.SubExpression where

import Prelude

import Data.Array (delete, filter, find, foldr, mapWithIndex, reverse, sortBy)
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
joinCommonSubExpressions expression = withCommonSubExpressions
  where
  commonSubExpressionsOrderedByOccurances = reverse $ sortByOccurances $ filterSingleOccurances $ countOccurances expression
  variableDefinitions = mapWithIndex assignVariableName commonSubExpressionsOrderedByOccurances
  withCommonSubExpressions = foldr addSubExpressions (expression) variableDefinitions 

addSubExpressions :: VariableExpression -> Expression -> Expression
addSubExpressions target parentExpression = ExpressionLet target.name target.expression (cleanParent target parentExpression)

cleanParent :: VariableExpression -> Expression -> Expression
cleanParent target expression =
  if expression == target.expression then
    ExpressionVariable target.name
  else case expression of
    ExpressionUnary unaryOperation nestedExpression -> ExpressionUnary unaryOperation $ cleanParent target nestedExpression
    ExpressionBinary binaryOperation leftExpression rightExpression -> ExpressionBinary binaryOperation (cleanParent target leftExpression) (cleanParent target rightExpression)
    -- If the expression is not the target and not a binary or unary operation then it is already clean.
    _ -> expression

assignVariableName :: Int -> SubExpressionCount -> VariableExpression
assignVariableName index { expression } = { expression, name: "$" <> (show (index + 1)) }

countOccurances :: Expression -> SubExpressionCounter
countOccurances = countOccurancesWithMap []
  where
  countOccurancesWithMap :: SubExpressionCounter -> Expression -> SubExpressionCounter
  countOccurancesWithMap counter = case _ of
    ExpressionUnary unaryOperation expression -> countOccurancesWithMap newCounter expression
      where
      newCounter = addExpressionToCounter counter (ExpressionUnary unaryOperation expression)
    ExpressionBinary binaryOperation leftExpression rightExpression -> mergeCounters newCounter (mergeCounters leftCounter rightCounter)
      where
      newCounter = addExpressionToCounter counter (ExpressionBinary binaryOperation leftExpression rightExpression)

      leftCounter = countOccurances leftExpression

      rightCounter = countOccurances rightExpression
    -- We dont want to count literals or variables
    expression -> counter

type VariableExpression
  = { expression :: Expression, name :: VariableName }

type SubExpressionCount
  = { expression :: Expression, occurances :: Int }

type SubExpressionCounter
  = Array SubExpressionCount

mergeCounters :: SubExpressionCounter -> SubExpressionCounter -> SubExpressionCounter
mergeCounters otherCounter = map (mergeWith otherCounter)

mergeWith :: SubExpressionCounter -> SubExpressionCount -> SubExpressionCount
mergeWith otherCounter count = count { occurances = count.occurances + countInOther }
  where
  countInOther = getOccurances otherCounter count.expression

filterSingleOccurances :: SubExpressionCounter -> SubExpressionCounter
filterSingleOccurances = filter (\count -> count.occurances == 1)

sortByOccurances :: SubExpressionCounter -> SubExpressionCounter
sortByOccurances = sortBy compareOccurances

compareOccurances :: SubExpressionCount -> SubExpressionCount -> Ordering
compareOccurances a b = compare (a.occurances) (b.occurances)

getOccurances :: SubExpressionCounter -> Expression -> Int
getOccurances counter expression = case lookupCount counter expression of
  Just count -> count.occurances
  _ -> 0

addExpressionToCounter :: SubExpressionCounter -> Expression -> SubExpressionCounter
addExpressionToCounter counter expression = case lookupCount counter expression of
  Just count -> (delete count counter) <> [ ({ expression, occurances: count.occurances + 1 }) ]
  Nothing -> counter <> [ { expression, occurances: 0 } ]

lookupCount :: SubExpressionCounter -> Expression -> Maybe SubExpressionCount
lookupCount counter expression = find (isThisExpression expression) counter

isThisExpression :: Expression -> SubExpressionCount -> Boolean
isThisExpression target { expression, occurances } = expression == target
