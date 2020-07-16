module FFI.Timer where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Unit (Test, failure)

foreign import processTime :: Effect Number

foreign import processTimeElapsedSince :: Number -> Effect Number

performanceTestFinishInMS :: Number -> Test -> Test
performanceTestFinishInMS time job = do
  start <- liftEffect processTime
  job
  elapsed <- liftEffect $ processTimeElapsedSince start
  when (elapsed > time) $ failure $ "expected completion in " <> (show time) <> "ms, actually completed in " <> (show elapsed) <> "ms"
