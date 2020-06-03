module Components.ExpressionInput.Controller where

import Expression.Error (Expect)
import Expression.Simplifier (simplify)
import Expression.Parser (parse)
import Expression.Syntax (Expression)

type ExpressionInputController
  = { parse :: String -> Expect Expression
      , simplify :: Expression -> Expression
    }

expressionInputController :: ExpressionInputController
expressionInputController = { parse, simplify }