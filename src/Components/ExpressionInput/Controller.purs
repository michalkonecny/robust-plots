module Components.ExpressionInput.Controller where

import Prelude
import Expression.Error (Expect)
import Expression.Parser (parse)
import Expression.Simplifier (simplify)
import Expression.SubExpression (joinCommonSubExpressions)
import Expression.Syntax (Expression)

type ExpressionInputController
  = { parse :: String -> Expect Expression
      , clean :: Expression -> Expression
    }

expressionInputController :: ExpressionInputController
expressionInputController = { parse, clean: simplify >>> joinCommonSubExpressions }