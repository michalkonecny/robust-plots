module Plot.Commands where

import Expression.Syntax (Expression)
import Types (XYBounds, Bounds)

data PlotCommand
  = Empty XYBounds
  | RoughPlot XYBounds Expression String
  | RobustPlot XYBounds Bounds Expression String

roughPlot :: XYBounds -> Expression -> String -> PlotCommand
roughPlot = RoughPlot 

robustPlot :: XYBounds -> Expression -> String -> PlotCommand
robustPlot bounds expression label = RobustPlot bounds bounds.xBounds expression label

clear :: XYBounds -> PlotCommand
clear = Empty

isPlotExpression :: PlotCommand -> Boolean
isPlotExpression (RoughPlot _ _ _) = true
isPlotExpression (Empty _) = false
isPlotExpression (RobustPlot _ _ _ _) = true