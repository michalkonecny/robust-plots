module Plot.PlotController where

import Prelude
import Data.Either (Either(..))
import Draw.Commands (DrawCommand)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff, nonCanceler)
import Plot.Commands (PlotCommand(..))
import Plot.GridLines (clearAndDrawGridLines)
import Plot.RobustPlot (drawRobustPlot)
import Plot.RoughPlot (drawRoughPlot)
import Types (Size)

computePlotAsync :: Size -> PlotCommand -> Aff (DrawCommand Unit)
computePlotAsync canvasSize plot = makeAff $ runComputation canvasSize plot

runComputation :: Size -> PlotCommand -> (Either Error (DrawCommand Unit) -> Effect Unit) -> Effect Canceler
runComputation canvasSize commands callback = do
  callback $ Right $ runCommand canvasSize commands
  pure nonCanceler

runCommand :: Size -> PlotCommand -> DrawCommand Unit
runCommand _ (Empty bounds) = clearAndDrawGridLines bounds

runCommand canvasSize (RoughPlot bounds expression label) = drawRoughPlot canvasSize bounds expression label

runCommand canvasSize (RobustPlot bounds expression domainSegments label) = drawRobustPlot canvasSize bounds expression domainSegments label
