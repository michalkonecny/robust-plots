module Expression.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Int (toNumber)
import Expression.Error (Expect, parseError)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)
import Text.Parsing.Parser.Expr (OperatorTable, Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser)
import Text.Parsing.Parser.Language (emptyDef)

token :: TokenParser
token = makeTokenParser emptyDef

type P a
  = Parser String a

brackets :: forall a. P a -> P a
brackets = token.parens

reservedOp :: String -> P Unit
reservedOp = token.reservedOp

identifier :: P String
identifier = token.identifier

literal :: P Expression
literal = ExpressionLiteral <$> toLiteral <$> token.naturalOrFloat

toLiteral :: Either Int Number -> Number
toLiteral (Right number) = number
toLiteral (Left integer) = toNumber integer

variable :: P Expression
variable = ExpressionVariable <$> identifier

term :: P Expression -> P Expression
term p = literal <|> brackets p <|> variable

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
