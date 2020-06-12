module Expression.Error where

import Prelude
import Expression.Syntax (VariableName)

import Data.Either (Either(..))

data Error = ParseError String
           | UnknownValue VariableName
           | MultipleErrors String

instance showError :: Show Error where
  show (ParseError s) = "Parse error: " <> s
  show (UnknownValue n) = "Unknown value: " <> n
  show (MultipleErrors msg) = msg     

derive instance eqError :: Eq Error

type Expect a = Either Error a

throw :: forall a. Error -> Expect a
throw = Left

parseError :: forall a. String -> Expect a
parseError = throw <<< ParseError

unknownValue :: forall a. VariableName -> Expect a
unknownValue = throw <<< UnknownValue

multipleErrors :: forall a. String -> Expect a
multipleErrors = throw <<< MultipleErrors