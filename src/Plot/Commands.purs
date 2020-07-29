module Plot.Commands where

import Data.Tuple (Tuple)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx)
import Types (XYBounds, Bounds)

data PlotCommand
  = Empty XYBounds
  | RoughFunctionPlot XYBounds Expression
  | RobustFunctionPlot XYBounds Expression (Array (Tuple Depth Approx)) Number
  | RoughParametricPlot XYBounds Bounds Expression Expression
  | RobustParametricPlot XYBounds Expression Expression (Array (Tuple Depth Approx)) Number

type Depth
  = Int

roughPlot :: XYBounds -> Expression -> PlotCommand
roughPlot = RoughFunctionPlot

roughParametricPlot :: XYBounds -> Bounds -> Expression -> Expression -> PlotCommand
roughParametricPlot = RoughParametricPlot

clear :: XYBounds -> PlotCommand
clear = Empty

isPlotExpression :: PlotCommand -> Boolean
isPlotExpression (RoughFunctionPlot _ _) = true

isPlotExpression (Empty _) = false

isPlotExpression (RobustFunctionPlot _ _ _ _) = true

isPlotExpression (RoughParametricPlot _ _ _ _) = true

isPlotExpression (RobustParametricPlot _ _ _ _ _) = true
