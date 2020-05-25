module Plot.PlotController where

import Prelude
import Draw.Commands (DrawCommand)
import Draw.Actions (drawPlotLine)
import Plot.Commands (PlotCommand(..))
import Data.Traversable (for_)
import Data.Array ((..), zipWith, tail)
import Data.Either (Either(..))
import Data.Int (floor, toNumber)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff, nonCanceler)
import Types (Size, XYBounds, Position)
import Plot.GridLines (clearAndDrawGridLines)

computePlotAsync :: Size -> PlotCommand -> Aff (DrawCommand Unit)
computePlotAsync canvasSize plot = makeAff $ runComputation canvasSize plot

runComputation :: Size -> PlotCommand -> (Either Error (DrawCommand Unit) -> Effect Unit) -> Effect Canceler
runComputation canvasSize (Plot shouldClear bounds func) callback = do
  callback $ Right
    $ do
        -- Computation for drawing plot here
        if shouldClear then
          clearAndDrawGridLines bounds
        else
          pure unit
        plotSimpleLine canvasSize bounds func
  pure nonCanceler

runComputation canvasSize (Empty bounds) callback = do
  callback $ Right $ clearAndDrawGridLines bounds
  pure nonCanceler

plotSimpleLine :: Size -> XYBounds -> (Number -> Number) -> DrawCommand Unit
plotSimpleLine canvasSize bounds func = for_ lines (\l -> drawPlotLine l.a l.b)
  where
  rangeX = bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = bounds.yBounds.upper - bounds.yBounds.lower

  points = map (toNumber >>> toCanvasPoint) $ 0 .. (floor canvasSize.width)

  lines = zipWith (\a b -> { a, b }) points (fromMaybe [] (tail points))

  toCanvasPoint :: Number -> Position
  toCanvasPoint canvasX = { x: canvasX, y: canvasY }
    where
    x = ((canvasX * rangeX) / canvasSize.width) + bounds.xBounds.lower

    y = func x

    canvasY = canvasSize.height - (((y - bounds.yBounds.lower) * canvasSize.height) / rangeY)
