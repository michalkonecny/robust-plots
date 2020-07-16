module FFI.Timer where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.Unit (failure)

foreign import processTime :: Effect Number

foreign import processTimeElapsedSince :: Number -> Effect Number

runPerformance :: Number -> Aff Unit -> Aff Unit
runPerformance time job = do
  start <- liftEffect processTime
  job
  e <- liftEffect $ processTimeElapsedSince start
  when (e > time) $ failure $ "expected completion in " <> (show time) <> "ms, actually completed in " <> (show e) <> "ms"
