module Misc.ExpectAff where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect.Aff (Aff, Error)

type ExpectAff a
  = Aff (Either Error a)

type ExpectArrayAff a
  = Aff (Array (Either Error a))

type MaybeExpectAff a
  = Aff (Maybe (Either Error a))

mapExpectAff :: forall a b. (a -> b) -> ExpectAff a -> ExpectAff b
mapExpectAff mapValue input = mapper <$> input
  where
  mapper :: Either Error a -> Either Error b
  mapper (Right v) = Right $ mapValue v

  mapper (Left e) = Left e

bindTo :: forall a b. ExpectAff a -> (a -> ExpectAff b) -> ExpectAff b
bindTo operation onSuccess = do
  resultOrError <- operation
  case resultOrError of
    Left error -> pure $ Left error
    Right result -> onSuccess result

pureRight :: forall a. a -> ExpectAff a
pureRight = pure <<< Right
