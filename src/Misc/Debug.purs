module Misc.Debug where

import Prelude
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)

unsafeLog :: forall a. String -> a -> a
unsafeLog m x =
  unsafePerformEffect do
    log m
    pure x

unsafeSpy :: forall a. Show a => String -> a -> a
unsafeSpy label x =
  unsafeLog (label <> ":" <> show x) x