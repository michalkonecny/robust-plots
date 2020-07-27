module Misc.ExpectAff where

import Prelude
import Data.Either (Either(..))
import Effect.Aff (Aff, Error)

type ExpectAff a
  = Aff (Either Error a)

type ExpectArrayAff a
  = Aff (Array (Either Error a))

mapExpectAff :: forall a b. (a -> b) -> ExpectAff a -> ExpectAff b
mapExpectAff mapValue input = mapper <$> input
  where
  mapper :: Either Error a -> Either Error b
  mapper (Right v) = Right $ mapValue v

  mapper (Left e) = Left e
