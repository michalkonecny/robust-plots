module Plot.RobustPlot where

import Prelude

import Data.Array (fold, length, tail, zipWith, (!!), (..))
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (isFinite)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Evaluator (evaluate)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsNumber, fromRationalBoundsPrec)
import IntervalArith.Misc (Rational, rationalToNumber, toRational)
import Plot.Helper (drawLabel)
import Types (XYBounds, Polygon, Position, Size)

segmentCount :: Int
segmentCount = 10

drawRobustPlot :: Size -> Int -> Int -> XYBounds -> Expression -> String -> DrawCommand Unit
drawRobustPlot canvasSize numberOfPlots index bounds expression label = drawCommands
  where
  f = evaluateWithX expression

  segmentEnclosures = plotEnclosures canvasSize bounds f

  labelPosition = toPosition $ fromMaybe [ { x: 0.0, y: 0.0 } ] $ segmentEnclosures !! ((length segmentEnclosures) / ((numberOfPlots + 1) * index))

  drawCommands = fold [ drawPlot segmentEnclosures, drawLabel label labelPosition ]

evaluateWithX :: Expression -> Approx -> Approx
evaluateWithX expression x = value
  where
  variableMap = [ Tuple "x" x ]

  value = case evaluate variableMap expression of
    Left _ -> zero -- TODO Handle any evaluation erros 
    Right v -> v

plotEnclosures :: Size -> XYBounds -> (Approx -> Approx) -> Array Polygon
plotEnclosures canvasSize bounds f = segmentEnclosures
  where
  rangeX = bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  domainBreakpoints = map (toRational >>> toDomainX) $ 0 .. segmentCount

  domainSegments = zipWith toRange domainBreakpoints (fromMaybe [] (tail domainBreakpoints))

  segmentWidth = rangeX / (toRational segmentCount)

  segmentEnclosures = map toCanvasEnclosure domainSegments 

  yLowerBound = rationalToNumber bounds.yBounds.lower

  canvasHeight = rationalToNumber canvasSize.height

  toRange :: Rational -> Rational -> Tuple Rational Rational
  toRange lower upper = Tuple lower upper

  toDomainX :: Rational -> Rational
  toDomainX segmentX = (segmentX * segmentWidth) + bounds.xBounds.lower 

  applyExpression :: Rational -> Rational -> Tuple Number Number
  applyExpression xLower xUpper = boundsNumber $ f $ fromRationalBoundsPrec 50 xLower xUpper

  toCanvasEnclosure :: Tuple Rational Rational -> Polygon
  toCanvasEnclosure (Tuple xLower xUpper) = polygon
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

    polygon = [ a, b, c, d, a ]

  toCanvasX :: Rational -> Number
  toCanvasX x = rationalToNumber $ ((x - bounds.xBounds.lower) * canvasSize.width) / rangeX

  toCanvasY :: Number -> Number
  toCanvasY y = safeCanvasY
    where
      canvasY = canvasHeight - (((y - yLowerBound) * canvasHeight) / rangeY)
      safeCanvasY = if isFinite canvasY then canvasY else if canvasY < zero then canvasHeight + one else -one

drawPlot :: Array Polygon -> DrawCommand Unit
drawPlot ploygons = drawEnclosure true ploygons

toPosition :: Polygon -> Position
toPosition polygon = { x, y }
  where
  x = (sum $ map (\p -> p.x) polygon) / (toNumber $ length polygon)

  y = (sum $ map (\p -> p.y) polygon) / (toNumber $ length polygon)
