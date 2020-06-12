module Plot.Commands where

import Expression.Syntax (Expression)
import Types (XYBounds)

data PlotCommand
  = Plot XYBounds Expression String
  | Empty XYBounds

plotExpression :: XYBounds -> Expression -> String -> PlotCommand
plotExpression = Plot 

clear :: XYBounds -> PlotCommand
clear = Empty

isPlotExpression :: PlotCommand -> Boolean
isPlotExpression (Plot _ _ _) = true
isPlotExpression (Empty _) = false