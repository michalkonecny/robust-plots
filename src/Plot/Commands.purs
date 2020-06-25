module Plot.Commands where

import Prelude
import Expression.Syntax (Expression)
import Types (XYBounds, Bounds)

data PlotCommand
  = Empty XYBounds
  | RoughPlot XYBounds Expression String
  | RobustPlot Int XYBounds Bounds Expression String

roughPlot :: XYBounds -> Expression -> String -> PlotCommand
roughPlot = RoughPlot

robustPlot :: Int -> XYBounds -> Expression -> String -> PlotCommand
robustPlot segmentCount bounds expression label = RobustPlot segmentCount bounds bounds.xBounds expression label

clear :: XYBounds -> PlotCommand
clear = Empty

isPlotExpression :: PlotCommand -> Boolean
isPlotExpression (RoughPlot _ _ _) = true

isPlotExpression (Empty _) = false

isPlotExpression (RobustPlot _ _ _ _ _) = true

derive instance plotCommandEq :: Eq PlotCommand

instance plotCommandShow :: Show PlotCommand where
  show (Empty bounds) = "Empty " <> (show bounds)
  show (RoughPlot bounds expression label) = "RoughPlot " <> (show bounds) <> " " <> (show expression) <> " " <> label
  show (RobustPlot segmentCount bounds fullXBounds expression label) = "RobustPlot " <> (show segmentCount) <> " " <> (show bounds) <> " " <> (show fullXBounds) <> " " <> (show expression) <> " " <> label
