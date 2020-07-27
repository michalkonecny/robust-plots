module Plot.Commands where

import Data.Tuple (Tuple)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx)
import Types (XYBounds)

data PlotCommand
  = Empty XYBounds
  | RoughFunctionPlot XYBounds Expression String
  | RobustPlot XYBounds Expression (Array (Tuple Depth Approx)) Number String

type Depth
  = Int

roughPlot :: XYBounds -> Expression -> String -> PlotCommand
roughPlot = RoughFunctionPlot

clear :: XYBounds -> PlotCommand
clear = Empty

isPlotExpression :: PlotCommand -> Boolean
isPlotExpression (RoughFunctionPlot _ _ _) = true

isPlotExpression (Empty _) = false

isPlotExpression (RobustPlot _ _ _ _ _) = true
