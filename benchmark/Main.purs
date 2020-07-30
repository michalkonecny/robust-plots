module Benchmark.Main where

import Prelude
import Benchmark.Plotter (benchPlot)
import Benchotron.UI.Console (runSuite)
import Effect (Effect)

main :: Effect Unit
main = runSuite [ benchPlot ]
