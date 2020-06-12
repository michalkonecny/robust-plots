module Expression.SubExpression where

import Prelude

import Data.Array (elem, foldr, fromFoldable, null, unsnoc)
import Data.Map (Map, filterKeys, lookup, toUnfoldable, values)
import Data.Maybe (Maybe(..))
import Data.Set (Set, empty, insert)
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Expression.Helper (mapMapEntries, setToMapWithIndex)
import Expression.Syntax (Expression(..), VariableName)
import Expression.VariableMap (VariableMap, lookup) as VM

removeSubExpressions :: Expression -> Expression
removeSubExpressions = removeSubExpressionsWithMap []
  where
  removeSubExpressionsWithMap :: (VM.VariableMap Expression) -> Expression -> Expression
  removeSubExpressionsWithMap variableMap = case _ of
    ExpressionVariable name -> case VM.lookup variableMap name of
      Just value -> value
      _ -> ExpressionVariable name
    ExpressionLet name expression parentExpression -> removeSubExpressionsWithMap (variableMap <> [ (Tuple name expression) ]) parentExpression
    expression -> expression

joinCommonSubExpressions :: Expression -> Expression
joinCommonSubExpressions e = ((buildExpression e) <<< orderDepencencies <<< substituteSubExpressions <<< subExpressionToVariableMap <<< splitSubExpressions) e

buildExpression :: Expression -> Array (Tuple Expression VariableName) -> Expression
buildExpression originalExpression orderedSubExpressions = case unsnoc orderedSubExpressions of
  Nothing -> originalExpression
  Just { init, last: (Tuple expression name) } -> foldr addLet expression init
  where
  addLet :: Tuple Expression VariableName -> Expression -> Expression
  addLet (Tuple expression name) subExpression = ExpressionLet name expression subExpression

orderDepencencies :: Map Expression VariableName -> Array (Tuple Expression VariableName)
orderDepencencies subExpressions = orderDepencenciesWithMap [] subExpressions
  where
  allSubExpressionVariables = values subExpressions

  orderDepencenciesWithMap :: Array VariableName -> Map Expression VariableName -> Array (Tuple Expression VariableName)
  orderDepencenciesWithMap extracted remaining = (toUnfoldable $ newExtracted) <> dependants
    where
    newExtracted = filterKeys canExtractExpression remaining

    newExtractedValueArray = fromFoldable $ values $ newExtracted

    newRemaining = filterKeys (not <<< canExtractExpression) remaining

    dependants =
      if null newExtractedValueArray then
        toUnfoldable remaining
      else
        orderDepencenciesWithMap newExtractedValueArray newRemaining

    canExtractExpression :: Expression -> Boolean
    canExtractExpression (ExpressionUnary _ expression) = canExtract expression

    canExtractExpression (ExpressionBinary _ leftExpression rightExpression) = canExtract leftExpression && canExtract rightExpression

    canExtractExpression expression = unsafeThrow "Invalid operation"

    canExtract :: Expression -> Boolean
    canExtract (ExpressionVariable n) = (elem n extracted) || not elem n allSubExpressionVariables

    canExtract (ExpressionUnary _ _) = unsafeThrow "Invalid operation"

    canExtract (ExpressionBinary _ _ _) = unsafeThrow "Invalid operation"

    canExtract expression = true

splitSubExpressions :: Expression -> Set Expression
splitSubExpressions = splitSubExpressionsWithMap empty
  where
  splitSubExpressionsWithMap :: Set Expression -> Expression -> Set Expression
  splitSubExpressionsWithMap subExpressions = case _ of
    ExpressionUnary unaryOperation expression -> splitSubExpressionsWithMap newCounter expression
      where
      newCounter = insert (ExpressionUnary unaryOperation expression) subExpressions
    ExpressionBinary binaryOperation leftExpression rightExpression -> rightCounter
      where
      newCounter = insert (ExpressionBinary binaryOperation leftExpression rightExpression) subExpressions

      leftCounter = splitSubExpressionsWithMap newCounter leftExpression

      rightCounter = splitSubExpressionsWithMap leftCounter rightExpression
    -- We dont want to count literals or variables
    expression -> subExpressions

subExpressionToVariableMap :: Set Expression -> Map Expression VariableName
subExpressionToVariableMap = setToMapWithIndex toVariableNameWithExpression

toVariableNameWithExpression :: Int -> Expression -> Tuple Expression VariableName
toVariableNameWithExpression index expression = Tuple expression ("$v" <> (show (index + 1)))

substituteSubExpressions :: Map Expression VariableName -> Map Expression VariableName
substituteSubExpressions subExpressions = mapMapEntries (substitute subExpressions) subExpressions

substitute :: Map Expression VariableName -> Tuple Expression VariableName -> Tuple Expression VariableName
substitute subExpressions (Tuple expression name) = Tuple e name
  where
  e = case expression of
    ExpressionUnary unaryOperation nestedExpression -> case lookup nestedExpression subExpressions of
      Nothing -> expression
      Just n -> ExpressionUnary unaryOperation $ ExpressionVariable n
    ExpressionBinary binaryOperation leftExpression rightExpression -> case lookup leftExpression subExpressions, lookup rightExpression subExpressions of
      Nothing, Nothing -> expression
      Just leftName, Just rightName -> ExpressionBinary binaryOperation (ExpressionVariable leftName) (ExpressionVariable rightName)
      Just leftName, _ -> ExpressionBinary binaryOperation (ExpressionVariable leftName) rightExpression
      _, Just rightName -> ExpressionBinary binaryOperation leftExpression (ExpressionVariable rightName)
    _ -> expression
