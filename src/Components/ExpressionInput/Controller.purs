module Components.ExpressionInput.Controller where
import Expression.Error (Expect)
import Expression.Parser (parse)
import Expression.Syntax (Expression)

type ExpressionInputController
  = { parse :: String -> Expect Expression
    }

expressionInputController :: ExpressionInputController
expressionInputController = { parse: parseExpression }
  where
  parseExpression :: String -> Expect Expression
  parseExpression = parse
