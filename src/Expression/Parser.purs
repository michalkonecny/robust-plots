module Expression.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Int (toNumber)
import Expression.Error (Expect, parseError)
import Expression.Simplifier (simplify)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser, fail)
import Text.Parsing.Parser.Combinators (lookAhead)
import Text.Parsing.Parser.Expr (OperatorTable, Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser)

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

variableOrUnaryFunctionCall :: P Expression -> P Expression
variableOrUnaryFunctionCall p = do
  idString <- identifier
  (lookAhead (string "(") *> functionCall idString)
    <|> (pure (ExpressionVariable idString))
  where
  functionCall idString = do
    expr <- brackets p
    case idString of
      "sin" -> pure $ ExpressionUnary Sine expr
      "cos" -> pure $ ExpressionUnary Cosine expr
      "tan" -> pure $ ExpressionUnary Tan expr
      "exp" -> pure $ ExpressionUnary Exp expr
      "log" -> pure $ ExpressionUnary Log expr
      "sqrt" -> pure $ ExpressionUnary Sqrt expr
      _ -> fail ("Unknown function: " <> idString)

term :: P Expression -> P Expression
term p = literal <|> brackets p <|> variableOrUnaryFunctionCall p

table :: OperatorTable Identity String Expression
table =
  [ [ Prefix (reservedOp "-" $> ExpressionUnary Neg)
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

parse :: String -> Expect Expression
parse input = case runParser input expressionParser of
  Left error -> parseError $ parseErrorMessage error
  Right experssion -> pure $ simplify experssion
