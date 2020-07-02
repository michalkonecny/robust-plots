module Plot.Commands where

import Prelude
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

derive instance plotCommandEq :: Eq PlotCommand

instance plotCommandShow :: Show PlotCommand where
  show (Empty bounds) = "Empty " <> (show bounds)
  show (RoughPlot bounds expression label) = "RoughPlot " <> (show bounds) <> " " <> (show expression) <> " " <> label
  show (RobustPlot bounds expression domainSegments label) = "RobustPlot " -- TODO
