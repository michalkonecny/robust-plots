module Plot.PlotController where

import Prelude
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Traversable (sum)
import Draw.Commands (DrawCommand)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff, nonCanceler)
import Plot.Commands (PlotCommand(..), isPlotExpression)
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

countPlots :: Array PlotCommand -> Int
countPlots = sum <<< map (fromEnum <<< isPlotExpression)

runCommand :: Size -> PlotCommand -> DrawCommand Unit
runCommand _ (Empty bounds) = clearAndDrawGridLines bounds

runCommand canvasSize (RoughPlot bounds expression label) = drawRoughPlot canvasSize bounds expression label

runCommand canvasSize (RobustPlot bounds fullXBounds expression label) = drawRobustPlot canvasSize fullXBounds bounds expression label
