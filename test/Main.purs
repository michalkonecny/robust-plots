module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)

import Test.IntervalArith (intervalArithTests)

main :: Effect Unit
main = do
  runTest do
    intervalArithTests
