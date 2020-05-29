module Plot.Commands where

import Expression.Syntax (Expression)
import Types (XYBounds)

data PlotCommand
  = Plot Boolean XYBounds Expression
  | Empty XYBounds

plot :: Boolean -> XYBounds -> Expression -> PlotCommand
plot clearCanvas bounds expression = Plot clearCanvas bounds expression

clear :: XYBounds -> PlotCommand
clear = Empty
