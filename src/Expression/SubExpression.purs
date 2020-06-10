module Expression.SubExpression where

import Prelude

import Data.Array (find, delete, sortBy, filter)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Expression.Syntax (Expression(..))
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

countOccurances :: Expression -> SubExpressionCounter
countOccurances = countOccurancesWithMap []
  where
  countOccurancesWithMap :: SubExpressionCounter -> Expression -> SubExpressionCounter
  countOccurancesWithMap counter = case _ of
    ExpressionUnary unaryOperation expression -> countOccurancesWithMap newCounter expression
      where
        newCounter = add counter (ExpressionUnary unaryOperation expression)
    ExpressionBinary binaryOperation leftExpression rightExpression -> mergeCounters newCounter (mergeCounters leftCounter rightCounter)
      where
        newCounter = add counter (ExpressionBinary binaryOperation leftExpression rightExpression)
        leftCounter = countOccurances leftExpression
        rightCounter = countOccurances rightExpression

    -- We dont want to count literals or variables
    expression -> counter

type SubExpressionCount = { expression :: Expression, occurances :: Int }

type SubExpressionCounter = Array SubExpressionCount

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

add :: SubExpressionCounter -> Expression -> SubExpressionCounter
add counter expression = case lookupCount counter expression of
  Just count -> (delete count counter) <> [ ({ expression, occurances: count.occurances + 1 }) ]
  Nothing -> counter <> [ { expression, occurances: 0 } ]

lookupCount :: SubExpressionCounter -> Expression -> Maybe SubExpressionCount
lookupCount counter expression = find (isThisExpression expression) counter

isThisExpression :: Expression -> SubExpressionCount -> Boolean
isThisExpression target { expression, occurances } = expression == target