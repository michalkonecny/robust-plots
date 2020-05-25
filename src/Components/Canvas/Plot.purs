module Components.Canvas.Plot where

import Types (XYBounds)
import Plotters (plot1)

data Plot
  = Plot1 Boolean XYBounds (Number -> Number)
  | Empty XYBounds

basicPlot :: Boolean -> XYBounds -> Plot
basicPlot clearCanvas bounds = Plot1 clearCanvas bounds plot1

clear :: XYBounds -> Plot
clear = Empty
