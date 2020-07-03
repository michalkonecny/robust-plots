module Plot.Commands where

import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx)
import Types (XYBounds)

data PlotCommand
  = Empty XYBounds
  | RoughPlot XYBounds Expression String
  | RobustPlot XYBounds Expression (Array Approx) String

roughPlot :: XYBounds -> Expression -> String -> PlotCommand
roughPlot = RoughPlot

clear :: XYBounds -> PlotCommand
clear = Empty

isPlotExpression :: PlotCommand -> Boolean
isPlotExpression (RoughPlot _ _ _) = true

isPlotExpression (Empty _) = false

isPlotExpression (RobustPlot _ _ _ _) = true
