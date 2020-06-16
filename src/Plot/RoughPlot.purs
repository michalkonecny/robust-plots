module Plot.RoughPlot where

import Prelude
import Data.Array (concat, fold, zipWith, (..), tail, (!!), length)
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Maybe (fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawPlotLine)
import Draw.Commands (DrawCommand)
import Expression.Differentiator (secondDifferentiate)
import Expression.Evaluator (roughEvaluate, presetConstants)
import Expression.Simplifier (simplify)
import Expression.Syntax (Expression)
import IntervalArith.Misc (Rational, rationalToNumber, toRational)
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

evaluateWithX :: Expression -> Rational -> Number
evaluateWithX expression x = value
  where
  variableMap = presetConstants <> [ Tuple "x" (rationalToNumber x) ]

  value = case roughEvaluate variableMap expression of
    Left _ -> 0.0
    Right v -> v

plotPoints :: Size -> XYBounds -> (Rational -> Number) -> (Rational -> Number) -> Array Position
plotPoints canvasSize bounds f f'' = points
  where
  rangeX = bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = bounds.yBounds.upper - bounds.yBounds.lower

  defaultRange = map (toRational >>> toDomainX) $ 0 .. (floor $ rationalToNumber canvasSize.width)

  changeInGradient = map f'' defaultRange

  points = map toCanvasPoint $ concat $ zipWith toRange changeInGradient defaultRange

  toRange :: Number -> Rational -> Array Rational
  toRange deltaGradient value =
    if (abs deltaGradient) < rationalToNumber (canvasSize.width / rangeX) then
      [ value ]
    else
      map (toSubRange <<< toRational) $ -5 .. 5
    where
    toSubRange :: Rational -> Rational
    toSubRange x = value + ((x * rangeX) / (canvasSize.width * toRational 10))

  toCanvasX :: Rational -> Number
  toCanvasX x = rationalToNumber $ ((x - bounds.xBounds.lower) * canvasSize.width) / rangeX

  toCanvasY :: Number -> Number
  toCanvasY y = (rationalToNumber canvasSize.height) - (((y - rationalToNumber bounds.yBounds.lower) * rationalToNumber canvasSize.height) / rationalToNumber rangeY)

  toDomainX :: Rational -> Rational
  toDomainX canvasX = ((canvasX * rangeX) / canvasSize.width) + bounds.xBounds.lower

  toCanvasPoint :: Rational -> Position
  toCanvasPoint x = { x: toCanvasX x, y: toCanvasY y }
    where
    y = f x

drawPlot :: Array Position -> DrawCommand Unit
drawPlot points = for_ lines (\l -> drawPlotLine l.a l.b)
  where
  lines = zipWith (\a b -> { a, b }) points (fromMaybe [] (tail points))
