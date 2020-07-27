module Components.Main.Helper where

import Prelude
import Components.Main.Types (State)
import Data.Array (fold, mapMaybe)
import Draw.Commands (DrawCommand)
import IntervalArith.Misc (rationalToNumber)
import Plot.Label (textHeight)
import Types (Position, Size)
import ViewModels.Expression (labelCommands, toMaybeDrawCommand)

foldDrawCommands :: State -> DrawCommand Unit
foldDrawCommands state = fold ([ state.clearPlot ] <> (mapMaybe toMaybeDrawCommand state.plots) <> [ labelCommands (isOffCanvasCheck state.input.size) state.plots ])

isOffCanvasCheck :: Size -> Position -> Boolean
isOffCanvasCheck canvasSize position = position.x < textHeight || position.x > width || position.y < textHeight || position.y > height
  where
  width = rationalToNumber canvasSize.width

  height = (rationalToNumber canvasSize.height) - 5.0
