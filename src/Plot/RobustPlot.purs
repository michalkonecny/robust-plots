module Plot.RobustPlot where

import Prelude
import Data.Array (catMaybes, fromFoldable)
import Data.List (List, singleton)
import Data.Maybe (Maybe(..))
import Data.Number (isFinite)
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, better, boundsA, boundsNumber, centreA, fromRationalBoundsPrec, fromRationalPrec, toNumber, upperBound)
import IntervalArith.Misc (Rational, rationalToNumber, toRational, two)
import Types (Polygon, Size, XYBounds)
import Plot.PlotEvaluator (ExpressionEvaluator, approxExpressionEvaluator)

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
  toCanvasEnclosure x = case evaluator.f x of
    Nothing -> Nothing
    Just approxValue -> case evaluator.f xMidPoint of
      Nothing -> Nothing
      Just midApproxValue -> case evaluator.f' x of
        Nothing -> Nothing
        Just approxGradient -> Just polygon
          where
          (Tuple yLower yUpper) = boundsA approxValue

          (Tuple yMidLower yMidUpper) = boundsA midApproxValue

          (Tuple yLowerGradient yUpperGradient) = boundsA approxGradient

          a = { x: canvasXLower, y: toCanvasY $ yMidLower - ((enclosureWidth * yUpperGradient) / twoA) }

          b = { x: canvasXLower, y: toCanvasY $ yMidUpper - ((enclosureWidth * yLowerGradient) / twoA) }

          c = { x: canvasXUpper, y: toCanvasY $ yMidUpper + ((enclosureWidth * yUpperGradient) / twoA) }

          d = { x: canvasXUpper, y: toCanvasY $ yMidLower + ((enclosureWidth * yLowerGradient) / twoA) }

          polygon = [ a, b, c, d, a ]
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

isOutSideCanvas :: XYBounds -> Approx -> Boolean
isOutSideCanvas bounds = better (fromRationalBoundsPrec 50 bounds.yBounds.lower bounds.yBounds.upper)

segmentDomain :: Number -> ExpressionEvaluator Number -> Rational -> Rational -> Array Approx
segmentDomain accuracyTarget evaluator l u = fromFoldable $ segementDomainF 0 l u
  where
  bisect :: Int -> Rational -> Rational -> Rational -> List Approx
  bisect depth lower mid upper = (segementDomainF (depth + one) lower mid) <> (segementDomainF (depth + one) mid upper)

  three = one + two

  segementDomainF :: Int -> Rational -> Rational -> List Approx
  segementDomainF depth lower upper = segments
    where
    x = fromRationalBoundsPrec 50 lower upper

    mid = (lower + upper) / two

    range = upper - lower

    a1 = rationalToNumber $ lower + (range / three)

    a2 = rationalToNumber $ lower + ((range * two) / three)

    segments =
      if depth < 5 then
        bisect depth lower mid upper
      else
        if depth >= 10 then
          singleton x
        else
          segmentBasedOnDerivative depth lower mid upper x (evaluator.f'' a1) (evaluator.f'' a2) (evaluator.f' $ rationalToNumber mid)

  segmentBasedOnDerivative :: Int -> Rational -> Rational -> Rational -> Approx -> Maybe Number -> Maybe Number -> Maybe Number -> List Approx
  segmentBasedOnDerivative depth lower mid upper x (Just a1) (Just a2) (Just b) =
    let
      w = rationalToNumber $ mid - lower

      a = (a1 + a2) / two

      h = if abs b > one then abs ((a * w * w) / b) else abs (a * w * w)
    in
      if h > accuracyTarget then
        bisect depth lower mid upper
      else
        singleton x

  segmentBasedOnDerivative _ _ _ _ x _ _ _ = singleton x

approxToRational :: Approx -> Rational
approxToRational = upperBound >>> toRational
