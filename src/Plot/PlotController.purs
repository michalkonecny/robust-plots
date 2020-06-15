module Plot.PlotController where

import Prelude

import Data.Array (fold, mapWithIndex)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Traversable (sum)
import Draw.Commands (DrawCommand)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff, nonCanceler)
import Plot.Commands (PlotCommand(..), isPlotExpression)
import Plot.GridLines (clearAndDrawGridLines)
import Plot.RoughPlot (drawRoughPlot)
import Types (Size)

computePlotAsync :: Size -> Array PlotCommand -> Aff (DrawCommand Unit)
computePlotAsync canvasSize plot = makeAff $ runComputation canvasSize plot

runComputation :: Size -> Array PlotCommand -> (Either Error (DrawCommand Unit) -> Effect Unit) -> Effect Canceler
runComputation canvasSize commands callback = do
  callback $ Right result
  pure nonCanceler
  where
  numberOfPlots = countPlots commands

  result = fold $ mapWithIndex (runCommand canvasSize numberOfPlots) commands

countPlots :: Array PlotCommand -> Int
countPlots = sum <<< map (fromEnum <<< isPlotExpression)

runCommand :: Size -> Int -> Int -> PlotCommand -> DrawCommand Unit
runCommand _ _ _ (Empty bounds) = clearAndDrawGridLines bounds

runCommand canvasSize numberOfPlots index (RoughPlot bounds expression label) = drawRoughPlot canvasSize numberOfPlots index bounds expression label
