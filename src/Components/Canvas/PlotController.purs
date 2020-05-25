module Components.Canvas.PlotController where

import Prelude

import Components.Canvas.Commands (DrawCommand)
import Components.Canvas.Commands.Actions (clearCanvas, drawPlotLine, drawXGridLine, drawYGridLine)
import Components.Canvas.Plot (Plot(..))
import Data.Traversable (for_)
import Data.Array ((..), zipWith, tail)
import Data.Decimal as D
import Data.Either (Either(..))
import Data.Int (floor, toNumber)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff, nonCanceler)
import Math (ceil)
import Types (Size, XYBounds, Position)

computePlotAsync :: Size -> Plot -> Aff (DrawCommand Unit)
computePlotAsync canvasSize plot = makeAff $ runComputation canvasSize plot

runComputation :: Size -> Plot -> (Either Error (DrawCommand Unit) -> Effect Unit) -> Effect Canceler
runComputation canvasSize (Plot1 shouldClear bounds plotter) callback = do
  callback $ Right
    $ do
        -- Computation for drawing plot here
        if shouldClear 
          then clearAndDrawGridLines bounds
          else pure unit

        plotSimpleLine canvasSize bounds (toCanvasPoint canvasSize plotter)
  pure nonCanceler

runComputation canvasSize (Empty bounds) callback = do
  callback $ Right $ clearAndDrawGridLines bounds
  pure nonCanceler

plotSimpleLine :: Size -> XYBounds -> (Number -> Position) -> DrawCommand Unit
plotSimpleLine canvasSize bounds canvasPlotter = do
  let 
    range = bounds.xBounds.upper - bounds.xBounds.lower
    interval = ceil (range / canvasSize.width)
    xValues = map (\p -> (toNumber p) * interval) $ 0 .. (floor canvasSize.width)
    points = map canvasPlotter xValues
    lines = zipWith (\a b -> { a, b }) points (fromMaybe [] (tail points))
  
  for_ lines (\l -> drawPlotLine l.a l.b)

clearAndDrawGridLines :: XYBounds -> DrawCommand Unit
clearAndDrawGridLines bounds = do
  clearCanvas
  drawXGridLines bounds
  drawYGridLines bounds  

toCanvasPoint :: Size -> (Number -> Number) -> Number -> Position
toCanvasPoint canvasSize func x = { x, y }
  where
    y = canvasSize.height - func x

drawXGridLines :: XYBounds -> DrawCommand Unit
drawXGridLines bounds = for_ xGuidePoints draw
  where
  range = bounds.xBounds.upper - bounds.xBounds.lower

  lineCount = 20.0

  x1 = bounds.xBounds.lower

  xGuidePoints = map (toGuidePoints x1 range lineCount) $ 0 .. (floor lineCount)

  draw :: { component :: Number, value :: Number } -> DrawCommand Unit
  draw { component, value } = drawXGridLine component value range

drawYGridLines :: XYBounds -> DrawCommand Unit
drawYGridLines bounds = for_ yGuidePoints draw
  where
  range = bounds.yBounds.upper - bounds.yBounds.lower

  lineCount = 20.0

  y1 = bounds.yBounds.lower

  yGuidePoints = map (toGuidePoints y1 range lineCount) $ 0 .. (floor lineCount)

  draw :: { component :: Number, value :: Number } -> DrawCommand Unit
  draw { component, value } = drawYGridLine component value range

toGuidePoints :: Number -> Number -> Number -> Int -> { component :: Number, value :: Number }
toGuidePoints offset range lineCount index = { component, value }
  where
  numberIndex = toNumber index

  component = (numberIndex * range) / lineCount

  value = to3SignificantDigits $ component + offset

toSignificantDigits :: Int -> Number -> Number
toSignificantDigits digits = D.toNumber <<< (D.toSignificantDigits digits) <<< D.fromNumber

to3SignificantDigits :: Number -> Number
to3SignificantDigits = toSignificantDigits 3
