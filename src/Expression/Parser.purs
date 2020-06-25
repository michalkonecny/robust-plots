module Expression.Parser where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.BigInt (pow)
import Data.Char.Unicode (digitToInt)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Identity (Identity)
import Data.List (List, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Ratio ((%))
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Expression.Error (Expect, parseError)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))
import IntervalArith.Misc (Rational, Integer, big, toRational)
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser, fail)
import Text.Parsing.Parser.Combinators (lookAhead, notFollowedBy, many1Till)
import Text.Parsing.Parser.Expr (OperatorTable, Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Token (TokenParser, digit, makeTokenParser)

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

literalExpression :: P Expression
literalExpression = ExpressionLiteral <$> literal

sign :: P (Rational -> Rational)
sign = (char '-' $> negate) <|> (char '+' $> identity) <|> pure identity

literal :: P Rational
literal = do
  addSign <- sign 
  integer <- token.integer
  let
    rationalInteger = toRational $ abs integer
  (lookAhead (char '.') *> asRational addSign rationalInteger) <|> (pure $ addSign rationalInteger)

asRational :: (Rational -> Rational) -> Rational -> P Rational
asRational addSign wholeNumber = do
  _ <- token.dot
  decimalPlaces <- many1Till digitToInteger isNotDigit
  pure $ addSign $ foldIntoRational wholeNumber decimalPlaces

digitToInteger :: P Integer
digitToInteger = digit >>= fromChar >>> big >>> pure

fromChar :: Char -> Int
fromChar char = case digitToInt char of
  Nothing -> unsafeThrow "Cannot convert 'non-integer' Char to Int"
  Just value -> value

isNotDigit :: P Unit
isNotDigit = notFollowedBy digit

-- | Folds a `List` of decimal values into a `Rational` starting at a base `Rational` value. This only works with 
-- | positive values and hence the sign should be handled outside this function.
foldIntoRational :: Rational -> List Integer -> Rational
foldIntoRational base decimalPlaces = foldl folder base $ mapWithIndex Tuple decimalPlaces
  where
  folder :: Rational -> (Tuple Int Integer) -> Rational
  folder accumulator (Tuple index element) = accumulator + (element % newDenominator)
    where
    newDenominator = pow (big 10) (big (index + one))

variableOrUnaryFunctionCall :: P Expression -> P Expression
variableOrUnaryFunctionCall p = do
  idString <- identifier
  (lookAhead (char '(') *> functionCall idString) <|> (pure (ExpressionVariable idString))
  where
  functionCall :: String -> P Expression
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
term p = literalExpression <|> brackets p <|> variableOrUnaryFunctionCall p

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
  Right experssion -> pure $ experssion
