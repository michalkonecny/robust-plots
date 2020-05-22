module Components.Canvas.Plot where

import Types (XYBounds)

data Plot
  = Plot1 Boolean XYBounds
  | Empty XYBounds

basicPlot :: Boolean -> XYBounds -> Plot
basicPlot clearCanvas bounds = Plot1 clearCanvas bounds

clear :: XYBounds -> Plot
clear = Empty
