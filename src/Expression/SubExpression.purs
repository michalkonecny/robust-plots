module Expression.SubExpression where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Expression.Error (Expect, unknownValue)
import Expression.Syntax (Expression(..))
import Expression.VariableMap (VariableMap, lookup)

removeSubExpressions :: (VariableMap Expression) -> Expression -> Expect Expression
removeSubExpressions variableMap = case _ of
  ExpressionVariable name -> case lookup variableMap name of
    Just value -> pure value
    _ -> unknownValue name
  ExpressionLet name expression parentExpression -> removeSubExpressions (variableMap <> [ (Tuple name expression) ]) parentExpression
  expression -> pure expression
