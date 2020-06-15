module Plot.Helper where

import Prelude
import Data.Array (length, tail, zipWith, (!!))
import Data.Maybe (fromMaybe)
import Data.Traversable (for_)
import Draw.Actions (drawPlotLine, drawText)
import Draw.Color (rgba)
import Draw.Commands (DrawCommand)
import Types (Position)

drawLabel :: String -> Array Position -> Int -> Int -> DrawCommand Unit
drawLabel label points numberOfPlots index = drawText color ("f(x)=" <> label) 20.0 labelPosition
  where
  pointIndex = (length points) / (numberOfPlots + 1)

  color = rgba 255.0 0.0 0.0 1.0

  labelPosition = fromMaybe { x: 0.0, y: 0.0 } $ points !! (pointIndex * index)

drawPlot :: Array Position -> DrawCommand Unit
drawPlot points = for_ lines (\l -> drawPlotLine l.a l.b)
  where
  lines = zipWith (\a b -> { a, b }) points (fromMaybe [] (tail points))
