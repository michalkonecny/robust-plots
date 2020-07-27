module Plot.PlotController where

import Prelude
import Data.Either (Either(..))
import Draw.Commands (DrawCommand)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff, nonCanceler)
import Effect.Console (log)
import Effect.Exception (try)
import Plot.Commands (PlotCommand(..))
import Plot.GridLines (clearAndDrawGridLines)
import Plot.RobustPlot (drawRobustPlot)
import Plot.RoughFunctionPlot (drawRoughPlot)
import Plot.RoughParametricPlot (drawRoughParametricPlot)
import Types (Size)

computePlotAsync :: Size -> PlotCommand -> Aff (Either Error (DrawCommand Unit))
computePlotAsync canvasSize plot = makeAff $ runComputation canvasSize plot

runComputation :: Size -> PlotCommand -> (Either Error (Either Error (DrawCommand Unit)) -> Effect Unit) -> Effect Canceler
runComputation canvasSize commands callback = do
  result <-
    try
      $ do
          log "Computing..."
          pure $ runCommand canvasSize commands
  callback $ Right result
  pure nonCanceler

runCommand :: Size -> PlotCommand -> DrawCommand Unit
runCommand _ (Empty bounds) = clearAndDrawGridLines bounds

runCommand canvasSize (RoughFunctionPlot bounds expression) = drawRoughPlot canvasSize bounds expression

runCommand canvasSize (RobustPlot bounds expression domainSegments accuracyTarget) = drawRobustPlot canvasSize bounds expression domainSegments accuracyTarget

runCommand canvasSize (RoughParametricPlot bounds domain xExpression yExpression) = drawRoughParametricPlot canvasSize bounds domain xExpression yExpression
