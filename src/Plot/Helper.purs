module Plot.Helper where

import Data.Maybe (Maybe(..))
import Plot.Commands (PlotCommand(..))
import Types (XYBounds)

updatePlotCommandBounds :: XYBounds -> Maybe PlotCommand -> PlotCommand
updatePlotCommandBounds bounds (Just (Plot _ _ func)) = Plot true bounds func

updatePlotCommandBounds bounds _ = Empty bounds
