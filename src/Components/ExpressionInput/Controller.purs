module Components.ExpressionInput.Controller where

import Prelude
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
    , check :: Expression -> Expect Number
    }

expressionInputController :: ExpressionInputController
expressionInputController = { parse, clean: simplify >>> joinCommonSubExpressions, check }

check :: Expression -> Expect Number
check expression = roughEvaluate (presetConstants <> [ Tuple "x" 0.0 ]) expression
