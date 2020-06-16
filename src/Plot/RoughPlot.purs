module Plot.RoughPlot where

import Prelude
import Data.Array (concat, fold, zipWith, (..), tail, (!!), length)
import Data.Either (Either(..))
import Data.Int (floor, toNumber)
import Data.Maybe (fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawPlotLine)
import Draw.Commands (DrawCommand)
import Expression.Differentiator (secondDifferentiate)
import Expression.Evaluator (roughEvaluate, presetConstants)
import Expression.Simplifier (simplify)
import Expression.Syntax (Expression)
import IntervalArith.Misc (rationalToNumber)
import Math (abs)
import Plot.Helper (drawLabel)
import Types (Size, XYBounds, Position)

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
    Left _ -> zero
    Right v -> v

plotPoints :: Size -> XYBounds -> (Number -> Number) -> (Number -> Number) -> Array Position
plotPoints canvasSize bounds f f'' = points
  where
  rangeX = rationalToNumber $ bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  width = rationalToNumber canvasSize.width

  height = rationalToNumber canvasSize.height

  xLower = rationalToNumber bounds.xBounds.lower

  yLower = rationalToNumber bounds.yBounds.lower

  defaultRange = map (toNumber >>> toDomainX) $ 0 .. (floor width)

  changeInGradient = map f'' defaultRange

  points = map toCanvasPoint $ concat $ zipWith toRange changeInGradient defaultRange

  toRange :: Number -> Number -> Array Number
  toRange deltaGradient value =
    if (abs deltaGradient) < (width / rangeX) then
      [ value ]
    else
      map (toNumber >>> toSubRange) $ -5 .. 5
    where
    toSubRange :: Number -> Number
    toSubRange x = value + ((x * rangeX) / (width * 10.0))

  toCanvasX :: Number -> Number
  toCanvasX x = ((x - xLower) * width) / rangeX

  toCanvasY :: Number -> Number
  toCanvasY y = height - (((y - yLower) * height) / rangeY)

  toDomainX :: Number -> Number
  toDomainX canvasX = ((canvasX * rangeX) / width) + xLower

  toCanvasPoint :: Number -> Position
  toCanvasPoint x = { x: toCanvasX x, y: toCanvasY y }
    where
    y = f x

drawPlot :: Array Position -> DrawCommand Unit
drawPlot points = for_ lines (\l -> drawPlotLine l.a l.b)
  where
  lines = zipWith (\a b -> { a, b }) points (fromMaybe [] (tail points))
