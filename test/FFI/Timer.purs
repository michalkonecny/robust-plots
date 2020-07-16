module FFI.Timer where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Unit (failure, success)

foreign import runPerformanceWith :: (String -> Aff Unit) -> Aff Unit -> Number -> Effect Unit -> Aff Unit

runPerformance :: Number -> Aff Unit -> Aff Unit
runPerformance time = runPerformanceWith failure success time <<< launchAff_