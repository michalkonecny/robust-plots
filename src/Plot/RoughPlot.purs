module Plot.RoughPlot where

import Prelude
import Data.Array (concat, fold, zipWith, (..), tail, (!!), length)
import Data.Either (Either(..))
import Data.Int (floor, toNumber)
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Expression.Differentiator (secondDifferentiate)
import Expression.Evaluator (roughEvaluate, presetConstants)
import Expression.Simplifier (simplify)
import Expression.Syntax (Expression)
import Math (abs)
import Plot.Helper (drawLabel)
import Types (Size, XYBounds, Position)
import Data.Maybe (fromMaybe)
import Data.Traversable (for_)
import Draw.Actions (drawPlotLine)

drawRoughPlot :: Size -> Int -> Int -> XYBounds -> Expression -> String -> DrawCommand Unit
drawRoughPlot canvasSize numberOfPlots index bounds expression label = drawCommands
  where
  f = evaluateWithX expression

  f'' = (evaluateWithX <<< simplify <<< secondDifferentiate) expression

  points = plotPoints canvasSize bounds f f''

  labelPosition = fromMaybe { x: 0.0, y: 0.0 } $ points !! ((length points) / ((numberOfPlots + 1) * index))

  drawCommands = fold [ drawPlot points, drawLabel label labelPosition ]

evaluateWithX :: Expression -> Number -> Number
evaluateWithX expression x = value
  where
  variableMap = presetConstants <> [ Tuple "x" x ]

  value = case roughEvaluate variableMap expression of
    Left _ -> 0.0
    Right v -> v

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