module Expression.Error where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Expression.Syntax (VariableName)

data Error
  = ParseError String
  | UnknownValue VariableName
  | EvaluationError String
  | UnsupportedOperation String
  | MultipleErrors (Array Error)

instance showError :: Show Error where
  show (ParseError s) = "Parse error: " <> s
  show (UnknownValue n) = "Unknown value: " <> n
  show (EvaluationError n) = "Evaluation error: " <> n
  show (UnsupportedOperation n) = "Unsupported operation: " <> n
  show (MultipleErrors errors) = joinWith " | " (map show errors)

derive instance eqError :: Eq Error

type Expect a
  = Either Error a

throw :: forall a. Error -> Expect a
throw = Left

parseError :: forall a. String -> Expect a
parseError = throw <<< ParseError

unknownValue :: forall a. VariableName -> Expect a
unknownValue = throw <<< UnknownValue

unsupportedOperation :: forall a. String -> Expect a
unsupportedOperation = throw <<< UnsupportedOperation

evaluationError :: forall a. String -> Expect a
evaluationError = throw <<< EvaluationError

multipleErrors :: forall a. Array Error -> Expect a
multipleErrors = throw <<< MultipleErrors

expectToMaybe :: forall a. Expect a -> Maybe a
expectToMaybe (Left _) = Nothing
expectToMaybe (Right v) = Just v