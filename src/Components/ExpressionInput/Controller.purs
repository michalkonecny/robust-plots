module Components.ExpressionInput.Controller where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.Tuple (Tuple(..))
import Expression.Error (Expect)
import Expression.Evaluator (presetConstants, roughEvaluate)
import Expression.Parser (parse)
import Expression.Simplifier (simplify)
import Expression.SubExpression (joinCommonSubExpressions)
import Expression.Syntax (Expression)

type ExpressionInputController
  = { parse :: String -> Expect Expression
    , clean :: Expression -> Expression
    , checkExpression :: Expression -> Expect Number
    , checkAccuracy :: String -> Either Number String
    }

expressionInputController :: ExpressionInputController
expressionInputController = { parse, clean: simplify >>> joinCommonSubExpressions, checkExpression, checkAccuracy }

checkExpression :: Expression -> Expect Number
checkExpression expression = roughEvaluate (presetConstants <> [ Tuple "x" 0.0 ]) expression

checkAccuracy :: String -> Either Number String
checkAccuracy accuracyString = case fromString accuracyString of
  Nothing -> Right "Failed to parse Accuracy"
  Just accuracy -> Left accuracy