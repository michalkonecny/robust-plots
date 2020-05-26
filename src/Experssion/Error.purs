module Expression.Error where

import Prelude
import Expression.Syntax (VariableName)

import Data.Either (Either(..))

data Error = ParseError String
           | UnknownValue VariableName
           | TheImpossibleHappened String

instance showError :: Show Error where
  show (ParseError s) = "Parse error: " <> s
  show (UnknownValue n) = "Unknown value: " <> n
  show (TheImpossibleHappened msg) = "The impossible happened: " <> msg     

type Expect a = Either Error a

throw :: forall a. Error -> Expect a
throw = Left

parseError :: forall a. String -> Expect a
parseError = throw <<< ParseError

unknownValue :: forall a. VariableName -> Expect a
unknownValue = throw <<< UnknownValue

theImpossibleHappened :: forall a. String -> Expect a
theImpossibleHappened = throw <<< TheImpossibleHappened