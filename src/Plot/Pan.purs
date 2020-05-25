module Plot.Pan where

import Prelude
import Data.Maybe (Maybe(..))
import Plot.Commands (PlotCommand(..))
import Types (Direction(..), XYBounds)

pan :: XYBounds -> Direction -> Maybe PlotCommand -> { plotCommand :: PlotCommand, newBounds :: XYBounds }
pan oldBounds panDirection previousCommand = { plotCommand, newBounds }
  where
  newBounds = panBounds oldBounds panDirection

  plotCommand = updatePlotCommandBounds newBounds previousCommand

panBounds :: XYBounds -> Direction -> XYBounds
panBounds bounds = case _ of
  Left -> bounds { xBounds { lower = bounds.xBounds.lower - xMovement, upper = bounds.xBounds.upper - xMovement } }
  Right -> bounds { xBounds { lower = bounds.xBounds.lower + xMovement, upper = bounds.xBounds.upper + xMovement } }
  Up -> bounds { yBounds { lower = bounds.yBounds.lower + yMovement, upper = bounds.yBounds.upper + yMovement } }
  Down -> bounds { yBounds { lower = bounds.yBounds.lower - yMovement, upper = bounds.yBounds.upper - yMovement } }
  where
  xRange = bounds.xBounds.upper - bounds.xBounds.lower

  yRange = bounds.yBounds.upper - bounds.yBounds.lower

  xMovement = (xRange / 10.0)

  yMovement = (yRange / 10.0)

updatePlotCommandBounds :: XYBounds -> Maybe PlotCommand -> PlotCommand
updatePlotCommandBounds bounds (Just (Plot _ _ func)) = Plot true bounds func

updatePlotCommandBounds bounds _ = Empty bounds
