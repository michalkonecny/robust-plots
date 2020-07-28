module Components.ExpressionInput.Controller where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.Tuple (Tuple(..))
import Expression.Error (Expect)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative, evaluateDerivative)
import Expression.Parser (parse)
import Expression.Simplifier (simplify)
import Expression.SubExpression (joinCommonSubExpressions)
import Expression.Syntax (Expression)

type FunctionExpressionInputController
  = { parse :: String -> Expect Expression
    , clean :: Expression -> Expression
    , checkExpression :: Expression -> Expect (ValueAndDerivative Number)
    , checkAccuracy :: String -> Either Number String
    }

expressionInputController :: FunctionExpressionInputController
expressionInputController = { parse, clean: simplify >>> joinCommonSubExpressions, checkExpression, checkAccuracy }

checkExpression :: Expression -> Expect (ValueAndDerivative Number)
checkExpression expression = evaluateDerivative [ Tuple "x" { value: 0.0, derivative: 1.0 } ] expression

checkAccuracy :: String -> Either Number String
checkAccuracy accuracyString = case fromString accuracyString of
  Nothing -> Right "Failed to parse Accuracy"
  Just accuracy -> Left accuracy
