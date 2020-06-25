module Plot.RobustPlot where

import Prelude
import Data.Array (length, tail, zipWith, (..))
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
import Types (Polygon, Position, Size, XYBounds, Bounds)

drawRobustPlot :: Int -> Size -> Bounds -> XYBounds -> Expression -> String -> DrawCommand Unit
drawRobustPlot segmentCount canvasSize fullXBounds bounds expression label = drawCommands
  where
  f = evaluateWithX expression

  segmentEnclosures = plotEnclosures segmentCount canvasSize fullXBounds bounds f

  drawCommands = drawPlot segmentEnclosures

evaluateWithX :: Expression -> Approx -> Approx
evaluateWithX expression x = value
  where
  variableMap = [ Tuple "x" x ]

  value = case evaluate variableMap expression of
    Left _ -> zero -- TODO Handle any evaluation erros 
    Right v -> v

plotEnclosures :: Int -> Size -> Bounds -> XYBounds -> (Approx -> Approx) -> Array Polygon
plotEnclosures segmentCount canvasSize fullXBounds bounds f = segmentEnclosures
  where
  rangeX = bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  fullRangeX = fullXBounds.upper - fullXBounds.lower

  domainBreakpoints = map (toRational >>> toDomainX) $ 0 .. segmentCount

  domainSegments = zipWith toRange domainBreakpoints (fromMaybe [] (tail domainBreakpoints))

  segmentWidth = rangeX / (toRational segmentCount)

  segmentEnclosures = map toCanvasEnclosure domainSegments

  yLowerBound = rationalToNumber bounds.yBounds.lower

  canvasHeight = rationalToNumber canvasSize.height

  halfRangeX = rangeX / (toRational 2)

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
  toCanvasX x = rationalToNumber $ ((x - fullXBounds.lower) * canvasSize.width) / fullRangeX

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
