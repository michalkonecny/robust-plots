module Plot.RobustPlot where

import Prelude

import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.Number (isFinite)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsA, boundsNumber, centreA, finite, fromRationalPrec, toNumber)
import IntervalArith.Misc (Rational, rationalToNumber, two)
import Plot.PlotEvaluator (ExpressionEvaluator, approxExpressionEvaluator)
import Types (Polygon, Size, XYBounds)

drawRobustPlot :: Size -> XYBounds -> Expression -> Array Approx -> String -> DrawCommand Unit
drawRobustPlot canvasSize bounds expression domainSegments label = drawCommands
  where
  expressionEvaluator = approxExpressionEvaluator expression

  segmentEnclosures = plotEnclosures canvasSize bounds domainSegments expressionEvaluator

  drawCommands = drawPlot segmentEnclosures

plotEnclosures :: Size -> XYBounds -> Array Approx -> ExpressionEvaluator Approx -> Array (Maybe Polygon)
plotEnclosures canvasSize bounds domainSegments evaluator = segmentEnclosures
  where
  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  rangeX = rationalToNumber $ bounds.xBounds.upper - bounds.xBounds.lower

  segmentEnclosures = map toCanvasEnclosure domainSegments

  yLowerBound = rationalToNumber bounds.yBounds.lower

  xLowerBound = rationalToNumber bounds.xBounds.lower

  canvasHeight = rationalToNumber canvasSize.height

  canvasWidth = rationalToNumber canvasSize.width

  toRange :: Rational -> Rational -> Tuple Rational Rational
  toRange lower upper = Tuple lower upper

  toCanvasEnclosure :: Approx -> Maybe Polygon
  toCanvasEnclosure x = case evaluator.f x, evaluator.f xMidPoint, evaluator.f' x of
    _, Just midApproxValue, Just approxGradient | finite midApproxValue && finite approxGradient  -> Just polygon
      where
      (Tuple yMidLower yMidUpper) = boundsA midApproxValue

      (Tuple yLowerGradient yUpperGradient) = boundsA approxGradient

      a = { x: canvasXLower, y: toCanvasY $ yMidLower - ((enclosureWidth * yUpperGradient) / twoA) }

      b = { x: canvasXLower, y: toCanvasY $ yMidUpper - ((enclosureWidth * yLowerGradient) / twoA) }

      c = { x: canvasXUpper, y: toCanvasY $ yMidUpper + ((enclosureWidth * yUpperGradient) / twoA) }

      d = { x: canvasXUpper, y: toCanvasY $ yMidLower + ((enclosureWidth * yLowerGradient) / twoA) }

      polygon = [ a, b, c, d, a ]
    Just approxValue, _, _ -> Just polygon
      where
      (Tuple yLower yUpper) = boundsA approxValue

      a = { x: canvasXLower, y: toCanvasY yUpper }

      b = { x: canvasXLower, y: toCanvasY yLower }

      c = { x: canvasXUpper, y: toCanvasY yLower }

      d = { x: canvasXUpper, y: toCanvasY yUpper }

      polygon = [ a, b, c, d, a ]
    _, _, _ -> Nothing
    where
    (Tuple xLower xUpper) = boundsNumber x

    (Tuple xLA xUA) = boundsA x

    xMidPoint = centreA x

    enclosureWidth = xUA - xLA

    twoA = fromRationalPrec 50 two

    canvasXLower = toCanvasX xLower

    canvasXUpper = toCanvasX xUpper

  toCanvasX :: Number -> Number
  toCanvasX x = ((x - xLowerBound) * canvasWidth) / rangeX

  toCanvasY :: Approx -> Number
  toCanvasY yApprox = safeCanvasY
    where
    y = toNumber yApprox

    canvasY = canvasHeight - (((y - yLowerBound) * canvasHeight) / rangeY)

    safeCanvasY = if isFinite canvasY then canvasY else if canvasY < zero then canvasHeight + one else -one

drawPlot :: Array (Maybe Polygon) -> DrawCommand Unit
drawPlot = (drawEnclosure true) <<< catMaybes
