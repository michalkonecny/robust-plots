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
import Types (Size, Bounds)

computePlotAsync :: Size -> Bounds -> PlotCommand -> Aff (DrawCommand Unit)
computePlotAsync canvasSize fullXBounds plot = makeAff $ runComputation canvasSize fullXBounds plot

runComputation :: Size -> Bounds -> PlotCommand -> (Either Error (DrawCommand Unit) -> Effect Unit) -> Effect Canceler
runComputation canvasSize fullXBounds commands callback = do
  callback $ Right $ runCommand canvasSize fullXBounds commands
  pure nonCanceler

countPlots :: Array PlotCommand -> Int
countPlots = sum <<< map (fromEnum <<< isPlotExpression)

runCommand :: Size -> Bounds -> PlotCommand -> DrawCommand Unit
runCommand _ _ (Empty bounds) = clearAndDrawGridLines bounds

runCommand canvasSize _ (RoughPlot bounds expression label) = drawRoughPlot canvasSize bounds expression label

runCommand canvasSize fullXBounds (RobustPlot bounds expression label) = drawRobustPlot canvasSize fullXBounds bounds expression label
