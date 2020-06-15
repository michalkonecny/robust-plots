module Plot.Commands where

import Expression.Syntax (Expression)
import Types (XYBounds)

data PlotCommand
  = RoughPlot XYBounds Expression String
  | Empty XYBounds

plotExpression :: XYBounds -> Expression -> String -> PlotCommand
plotExpression = RoughPlot 

clear :: XYBounds -> PlotCommand
clear = Empty

isPlotExpression :: PlotCommand -> Boolean
isPlotExpression (RoughPlot _ _ _) = true
isPlotExpression (Empty _) = false