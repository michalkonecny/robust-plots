module Plot.Commands where

import Plot.Functions (plot1)
import Types (XYBounds)

data PlotCommand
  = Plot Boolean XYBounds (Number -> Number)
  | Empty XYBounds

basicPlot :: Boolean -> XYBounds -> PlotCommand
basicPlot clearCanvas bounds = Plot clearCanvas bounds plot1

clear :: XYBounds -> PlotCommand
clear = Empty
