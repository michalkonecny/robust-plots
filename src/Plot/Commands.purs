module Plot.Commands where

import Expression.Syntax (Expression)
import Types (XYBounds)

data PlotCommand
  = Plot XYBounds Expression
  | Empty XYBounds

plotExpression :: XYBounds -> Expression -> PlotCommand
plotExpression bounds expression = Plot bounds expression

clear :: XYBounds -> PlotCommand
clear = Empty
