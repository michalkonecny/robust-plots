module Plot.Expression.Parser where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Identity (Identity)
import Plot.Expression.Syntax (BinaryOperation(..), Constant(..), Expression(..), UnaryOperation(..))
import Plot.Expression.Token (token)
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)
import Text.Parsing.Parser.Expr (OperatorTable, Assoc(..), Operator(..), buildExprParser)
import Plot.Expression.Error (Expect, parseError)
import Data.Either (Either(..))

type P a
  = Parser String a

brackets :: forall a. P a -> P a
brackets = token.parens

reservedOp :: String -> P Unit
reservedOp = token.reservedOp

reserved :: String -> P Unit
reserved = token.reserved

identifier :: P String
identifier = token.identifier

literal :: P Expression
literal = ExpressionLiteral <$> token.float

constant :: P Expression
constant = reserved "e" $> ExpressionConstant E <|> reserved "pi" $> ExpressionConstant Pi

variable :: P Expression
variable = ExpressionVariable <$> identifier

term :: P Expression -> P Expression
term p = brackets p <|> literal <|> constant

table :: OperatorTable Identity String Expression
table =
  [ [ Prefix (reservedOp "-" $> ExpressionUnary Neg)
    , Prefix (reservedOp "sin" $> ExpressionUnary Sine)
    , Prefix (reservedOp "cos" $> ExpressionUnary Cosine)
    , Prefix (reservedOp "tan" $> ExpressionUnary Tan)
    , Prefix (reservedOp "exp" $> ExpressionUnary Exp)
    , Prefix (reservedOp "log" $> ExpressionUnary Log)
    , Prefix (reservedOp "sqrt" $> ExpressionUnary Sqrt)
    , Infix (reservedOp "^" $> ExpressionBinary Power) AssocLeft
    ]
  , [ Infix (reservedOp "*" $> ExpressionBinary Times) AssocLeft
    , Infix (reservedOp "/" $> ExpressionBinary Divide) AssocLeft
    ]
  , [ Infix (reservedOp "+" $> ExpressionBinary Plus) AssocLeft
    , Infix (reservedOp "-" $> ExpressionBinary Minus) AssocLeft
    ]
  ]

expressionParser :: P Expression
expressionParser = fix (\p -> buildExprParser table (term p))

foldConstants :: Expression -> Expression
foldConstants (ExpressionUnary operation expression) = case foldConstants expression, operation of
  ExpressionLiteral value, Neg -> ExpressionLiteral (- value)
  foldedExpression, _ -> ExpressionUnary operation foldedExpression
foldConstants (ExpressionBinary operation leftExpression rightExpression) = case foldConstants leftExpression, foldConstants rightExpression, operation of
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Plus -> ExpressionLiteral (leftValue + rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Minus -> ExpressionLiteral (leftValue - rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Times -> ExpressionLiteral (leftValue * rightValue)
  ExpressionLiteral leftValue, ExpressionLiteral rightValue, Divide -> ExpressionLiteral (leftValue / rightValue)
  foldedLeftExpression, foldedRightExpression, _ -> ExpressionBinary operation foldedLeftExpression foldedRightExpression
foldConstants expression = expression

parse :: String -> Expect Expression
parse input = case runParser input expressionParser of
  Left error -> parseError $ parseErrorMessage error
  Right experssion -> pure $ foldConstants experssion
