module Plot.RobustPlot where

import Prelude
import Data.Array (fold, length, (..), (!!))
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Evaluator (evaluate)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsNumber, fromRationalBoundsPrec)
import IntervalArith.Misc (Rational, rationalToNumber, toRational)
import Plot.Helper (drawLabel)
import Types (XYBounds, Polygon, Position, Size)

drawRobustPlot :: Size -> Int -> Int -> XYBounds -> Expression -> String -> DrawCommand Unit
drawRobustPlot canvasSize numberOfPlots index bounds expression label = drawCommands
  where
  f = evaluateWithX expression

  points = plotPoints canvasSize bounds f

  labelPosition = toPosition $ fromMaybe [ { x: 0.0, y: 0.0 } ] $ points !! ((length points) / ((numberOfPlots + 1) * index))

  drawCommands = fold [ drawPlot points, drawLabel label labelPosition ]

evaluateWithX :: Expression -> Approx -> Approx
evaluateWithX expression x = value
  where
  variableMap = [ Tuple "x" x ]

  value = case evaluate variableMap expression of
    Left _ -> zero
    Right v -> v

plotPoints :: Size -> XYBounds -> (Approx -> Approx) -> Array Polygon
plotPoints canvasSize bounds f = points
  where
  segmentCount = 50

  rangeX = bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  defaultRange = map (toRational >>> toDomainX) $ 0 .. segmentCount

  segmentWidth = rangeX / (toRational segmentCount)

  points = map toCanvasPoint defaultRange

  yLowerBound = rationalToNumber bounds.yBounds.lower

  canvasHeight = rationalToNumber canvasSize.height

  toDomainX :: Rational -> Tuple Rational Rational
  toDomainX segmentX = Tuple lower upper
    where
    lower = (segmentX * segmentWidth) + bounds.xBounds.lower

    upper = lower + segmentWidth

  applyExpression :: Rational -> Rational -> Tuple Number Number
  applyExpression xLower xUpper = boundsNumber $ f $ fromRationalBoundsPrec 50 xLower xUpper

  toCanvasPoint :: Tuple Rational Rational -> Polygon
  toCanvasPoint (Tuple xLower xUpper) = polygon
    where
    (Tuple yLower yUpper) = applyExpression xLower xUpper

    canvasXLower = toCanvasX xLower

    canvasXUpper = toCanvasX xUpper

    canvasYLower = toCanvasY yLower

    canvasYUpper = toCanvasY yUpper

    a = { x: canvasXLower, y: canvasYUpper }

    b = { x: canvasXUpper, y: canvasYUpper }

    c = { x: canvasXUpper, y: canvasYLower }

    d = { x: canvasXLower, y: canvasYLower }

    polygon = [ a, b, c, d ]

  toCanvasX :: Rational -> Number
  toCanvasX x = rationalToNumber $ ((x - bounds.xBounds.lower) * canvasSize.width) / rangeX

  toCanvasY :: Number -> Number
  toCanvasY y = canvasHeight - (((y - yLowerBound) * canvasHeight) / rangeY)

drawPlot :: Array Polygon -> DrawCommand Unit
drawPlot ploygons = drawEnclosure true ploygons

toPosition :: Polygon -> Position
toPosition polygon = { x, y }
  where
  x = (sum $ map (\p -> p.x) polygon) / (toNumber $ length polygon)

  y = (sum $ map (\p -> p.y) polygon) / (toNumber $ length polygon)
