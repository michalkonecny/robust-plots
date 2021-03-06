module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.IntervalArith (intervalArithTests)
import Test.Expression (expressionTests)
import Test.Misc (miscTests)
import Test.Plot (plotTests)

main :: Effect Unit
main = do
  runTest do
    intervalArithTests
    expressionTests
    plotTests
    miscTests
