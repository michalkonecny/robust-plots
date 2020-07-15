module Plot.RobustPlot where

import Prelude
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (isFinite)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative2, evaluateDerivative2)
import Expression.Syntax (Expression)
import Expression.VariableMap (VariableMap)
import IntervalArith.Approx (Approx, boundsA, boundsNumber, centreA, fromRationalPrec, toNumber)
import IntervalArith.Approx.ExpLog (eA)
import IntervalArith.Approx.Pi (piA)
import IntervalArith.Misc (Rational, rationalToNumber, two)
import Types (Polygon, Size, XYBounds)

drawRobustPlot :: Size -> XYBounds -> Expression -> Array Approx -> String -> DrawCommand Unit
drawRobustPlot canvasSize bounds expression domainSegments label = drawCommands
  where
  precision = 50

  expressionEvaluator = evaluateWithX (constantsWithPrecision precision) expression

  segmentEnclosures = plotEnclosures canvasSize bounds domainSegments expressionEvaluator

  drawCommands = drawPlot segmentEnclosures

constantsWithPrecision :: Int -> VariableMap (ValueAndDerivative2 Approx)
constantsWithPrecision p =
  [ Tuple "e" { value: eA p, derivative: zero, derivative2: zero }
  , Tuple "pi" { value: piA p, derivative: zero, derivative2: zero }
  ]

evaluateWithX :: VariableMap (ValueAndDerivative2 Approx) -> Expression -> Approx -> Maybe (ValueAndDerivative2 Approx)
evaluateWithX constants expression x = value
  where
  variableMap = [ Tuple "x" { value: x, derivative: one, derivative2: zero } ] <> constants

  value = case evaluateDerivative2 variableMap expression of
    Left _ -> Nothing
    Right v -> Just v

plotEnclosures :: Size -> XYBounds -> Array Approx -> (Approx -> Maybe (ValueAndDerivative2 Approx)) -> Array (Maybe Polygon)
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
  toCanvasEnclosure x = case evaluator x, evaluator xMidPoint of
    Just approxValue, Just midApproxValue -> Just polygon
      where
      (Tuple yMidLower yMidUpper) = boundsA midApproxValue.value

      (Tuple yLowerGradient yUpperGradient) = boundsA approxValue.derivative

      a = { x: canvasXLower, y: toCanvasY $ yMidLower - ((enclosureWidth * yUpperGradient) / twoA) }

      b = { x: canvasXLower, y: toCanvasY $ yMidUpper - ((enclosureWidth * yLowerGradient) / twoA) }

      c = { x: canvasXUpper, y: toCanvasY $ yMidUpper + ((enclosureWidth * yUpperGradient) / twoA) }

      d = { x: canvasXUpper, y: toCanvasY $ yMidLower + ((enclosureWidth * yLowerGradient) / twoA) }

      polygon = [ a, b, c, d, a ]
    _, _ -> Nothing
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
