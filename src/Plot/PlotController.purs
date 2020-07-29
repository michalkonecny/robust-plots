module Plot.PlotController where

import Prelude

import Data.Either (Either(..))
import Draw.Commands (DrawCommand)
import Effect (Effect)
import Effect.Aff (Canceler, Error, makeAff, nonCanceler)
import Effect.Console (log)
import Effect.Exception (try)
import Misc.ExpectAff (ExpectAff)
import Plot.Commands (PlotCommand(..))
import Plot.GridLines (clearAndDrawGridLines)
import Plot.RobustFunctionPlot (drawRobustFunctionPlot)
import Plot.RobustParametricPlot (drawRobustParametricPlot)
import Plot.RoughFunctionPlot (drawRoughPlot)
import Plot.RoughParametricPlot (drawRoughParametricPlot)
import Types (Size)

computePlotAsync :: Size -> PlotCommand -> ExpectAff (DrawCommand Unit)
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

runCommand canvasSize (RobustFunctionPlot bounds expression domainSegments accuracyTarget) = drawRobustFunctionPlot canvasSize bounds expression domainSegments accuracyTarget

runCommand canvasSize (RoughParametricPlot bounds domain xExpression yExpression) = drawRoughParametricPlot canvasSize bounds domain xExpression yExpression

runCommand canvasSize (RobustParametricPlot bounds xExpression yExpression domainSegments accuracyTarget) = drawRobustParametricPlot canvasSize bounds xExpression yExpression  domainSegments accuracyTarget
