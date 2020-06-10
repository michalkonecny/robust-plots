module Expression.SubExpression where

import Prelude

import Data.Array (delete, filter, find, mapWithIndex, reverse, sortBy, uncons, partition)
import Data.Maybe (Maybe(..), isJust)
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
  Just { head, tail } -> result
    where 
      addedHeadVariable = addSubExpressions head expression
      newVariableDefinitions = map (\target -> target { expression = substitute head target.expression }) tail
      result = addSubExpressionsUntilConvergence newVariableDefinitions addedHeadVariable

addSubExpressions :: VariableExpression -> Expression -> Expression
addSubExpressions target parentExpression = ExpressionLet target.name target.expression (substitute target parentExpression)

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
mergeCounters a b = (map (mergeWith aInB) bInA) <> justInA <> justInB
  where
    { yes: aInB, no: justInA } = partition (\aTarget -> isJust (find (isThisExpression aTarget.expression) b)) a
    { yes: bInA, no: justInB } = partition (\bTarget -> isJust (find (isThisExpression bTarget.expression) a)) b

mergeWith :: SubExpressionCounter -> SubExpressionCount -> SubExpressionCount
mergeWith otherCounter count = count { occurances = count.occurances + countInOther }
  where
  countInOther = getOccurances otherCounter count.expression

filterSingleOccurances :: SubExpressionCounter -> SubExpressionCounter
filterSingleOccurances = filter (\count -> count.occurances > 1)

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
  Nothing -> counter <> [ { expression, occurances: 1 } ]

lookupCount :: SubExpressionCounter -> Expression -> Maybe SubExpressionCount
lookupCount counter expression = find (isThisExpression expression) counter

isThisExpression :: Expression -> SubExpressionCount -> Boolean
isThisExpression target { expression, occurances } = expression == target
