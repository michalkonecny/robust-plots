module Test.Expression.Helper where

import Prelude
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Expression.Error (Expect, Error)
import Test.Unit (failure)
import Test.Unit.Assert (equal)

expectValue :: forall a. Expect a -> (a -> Aff Unit) -> Aff Unit
expectValue expect check = case expect of
  Left error -> failure (show error)
  Right value -> check value

expectError :: forall a. Error -> Expect a -> Aff Unit
expectError expectedError expect = case expect of
  Left error -> equal expectedError error
  Right _ -> failure $ "Expected: " <> (show expectedError)
