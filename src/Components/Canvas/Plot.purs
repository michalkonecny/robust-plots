module Components.Canvas.Plot where

import Types (XYBounds)
import Plotters (plot1)

data Plot
  = Plot Boolean XYBounds (Number -> Number)
  | Empty XYBounds

basicPlot :: Boolean -> XYBounds -> Plot
basicPlot clearCanvas bounds = Plot clearCanvas bounds plot1

clear :: XYBounds -> Plot
clear = Empty
