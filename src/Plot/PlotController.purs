module Plot.PlotController where

import Prelude

import Data.Array (concat, fold, length, tail, zipWith, (!!), (..), mapWithIndex, filter)
import Data.Either (Either(..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_, sum)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawPlotLine, drawText)
import Draw.Color (rgba)
import Draw.Commands (DrawCommand)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff, nonCanceler)
import Expression.Differentiator (secondDifferentiate)
import Expression.Evaluator (evaluate, presetConstants)
import Expression.Simplifier (simplify)
import Expression.Syntax (Expression)
import Math (abs)
import Plot.Commands (PlotCommand(..), isPlotExpression)
import Plot.GridLines (clearAndDrawGridLines)
import Types (Size, XYBounds, Position)

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
countPlots commands = sum $ map (isPlotExpression >>> (\isPlot -> if isPlot then 1 else 0)) commands

evaluateWithX :: Expression -> Number -> Number
evaluateWithX expression x = value
  where
  variableMap = presetConstants <> [ Tuple "x" x ]

  value = case evaluate variableMap expression of
    Left _ -> 0.0
    Right v -> v

runCommand :: Size -> Int -> Int -> PlotCommand -> DrawCommand Unit
runCommand _ _ _ (Empty bounds) = clearAndDrawGridLines bounds

runCommand canvasSize numberOfPlots index (Plot bounds expression) = drawCommands
  where
  f = evaluateWithX expression

  f'' = evaluateWithX $ simplify $ secondDifferentiate expression

  points = filter (isWithinCanvas canvasSize) $ plotPoints canvasSize bounds f f''

  drawCommands = fold [ drawPlot points, label expression points numberOfPlots index ]

isWithinCanvas :: Size -> Position -> Boolean
isWithinCanvas canvasSize point = point.x >= 0.0 && point.x <= canvasSize.width + 1.0 && point.y >= 0.0 && point.y <= canvasSize.height + 1.0

label :: Expression -> Array Position -> Int -> Int -> DrawCommand Unit
label expression points numberOfPlots index = drawText color ("f(x)=" <> (show expression)) 20.0 labelPosition
  where
  pointIndex = (length points) / (numberOfPlots + 1)

  color = rgba 255.0 0.0 0.0 1.0

  labelPosition = fromMaybe { x: 0.0, y: 0.0 } $ points !! (pointIndex * index)

plotPoints :: Size -> XYBounds -> (Number -> Number) -> (Number -> Number) -> Array Position
plotPoints canvasSize bounds f f'' = points
  where
  rangeX = bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = bounds.yBounds.upper - bounds.yBounds.lower

  defaultRange = map (toNumber >>> toDomainX) $ 0 .. (floor canvasSize.width)

  changeInGradient = map f'' defaultRange

  points = map toCanvasPoint $ concat $ zipWith toRange changeInGradient defaultRange

  toRange :: Number -> Number -> Array Number
  toRange deltaGradient value =
    if (abs deltaGradient) < (canvasSize.width / rangeX) then
      [ value ]
    else
      map (toSubRange <<< toNumber) $ -5 .. 5
    where
    toSubRange :: Number -> Number
    toSubRange x = value + ((x * rangeX) / (canvasSize.width * 10.0))

  toCanvasX :: Number -> Number
  toCanvasX x = ((x - bounds.xBounds.lower) * canvasSize.width) / rangeX

  toCanvasY :: Number -> Number
  toCanvasY y = canvasSize.height - (((y - bounds.yBounds.lower) * canvasSize.height) / rangeY)

  toDomainX :: Number -> Number
  toDomainX canvasX = ((canvasX * rangeX) / canvasSize.width) + bounds.xBounds.lower

  toCanvasPoint :: Number -> Position
  toCanvasPoint x = { x: toCanvasX x, y: toCanvasY y }
    where
    y = f x

drawPlot :: Array Position -> DrawCommand Unit
drawPlot points = for_ lines (\l -> drawPlotLine l.a l.b)
  where
  lines = zipWith (\a b -> { a, b }) points (fromMaybe [] (tail points))

toMaybePlotCommand :: PlotCommand -> Maybe PlotCommand
toMaybePlotCommand (Empty _) = Nothing

toMaybePlotCommand p = Just p
