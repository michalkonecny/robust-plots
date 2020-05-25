module Plot.Zoom where

import Prelude
import Data.Maybe (Maybe)
import Plot.Commands (PlotCommand)
import Types (XYBounds)
import Plot.Helper (updatePlotCommandBounds)

zoom :: XYBounds -> Boolean -> Maybe PlotCommand -> { plotCommand :: PlotCommand, newBounds :: XYBounds }
zoom oldBounds isZoomIn previousCommand = { plotCommand, newBounds }
  where
  newBounds = zoomBounds oldBounds isZoomIn

  plotCommand = updatePlotCommandBounds newBounds previousCommand

zoomBounds :: XYBounds -> Boolean -> XYBounds
zoomBounds bounds isZoomIn = if isZoomIn 
  then bounds { 
      xBounds { lower = bounds.xBounds.lower + xMovement, upper = bounds.xBounds.upper - xMovement },
      yBounds { lower = bounds.yBounds.lower + yMovement, upper = bounds.yBounds.upper - yMovement }
      }
  else bounds { 
      xBounds { lower = bounds.xBounds.lower - xMovement, upper = bounds.xBounds.upper + xMovement },
      yBounds { lower = bounds.yBounds.lower - yMovement, upper = bounds.yBounds.upper + yMovement }
      }
  where
  xRange = bounds.xBounds.upper - bounds.xBounds.lower

  yRange = bounds.yBounds.upper - bounds.yBounds.lower

  xMovement = (xRange / 20.0)

  yMovement = (yRange / 20.0)