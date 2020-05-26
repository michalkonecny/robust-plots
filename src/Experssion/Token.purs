module Expression.Token where

import Control.Alt ((<|>))
import Text.Parsing.Parser.Token (LanguageDef, GenLanguageDef(..), TokenParser, makeTokenParser, letter, alphaNum)
import Text.Parsing.Parser.String (char, oneOf)

languageDefinition :: LanguageDef
languageDefinition =
  LanguageDef
    { commentStart: ""
    , commentEnd: ""
    , commentLine: ""
    , nestedComments: false
    , identStart: letter
    , identLetter: alphaNum <|> char '\''
    , opStart: oneOf [ '-', '+', '*', '/', '^' ]
    , opLetter: oneOf []
    , reservedNames: ["e", "pi"]
    , reservedOpNames: [ "+", "-", "*", "/", "sin", "cos", "tan", "exp", "log", "sqrt" ]
    , caseSensitive: true
    }

token :: TokenParser
token = makeTokenParser languageDefinition
