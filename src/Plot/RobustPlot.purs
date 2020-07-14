module Plot.RobustPlot where

import Prelude

import Data.Array (catMaybes, reverse, take)
import Data.Maybe (Maybe(..))
import Data.Number (isFinite)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsA, boundsNumber, centreA, finite, fromRationalPrec, lowerA, toNumber, upperA)
import IntervalArith.Approx.NumOrder ((!<=!), (!>=!))
import IntervalArith.Misc (Rational, rationalToNumber, two)
import Misc.Debug (unsafeSpy)
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
  {- overview:
      - try to compute f'(x)
        - if f''(x) is positive or negative, get f'(x) by endpoints
        - otherwise get f'(x) by midpoint and f''(x), or directly if f''(x) fails to compute
      - if f'(x) cannot be computed, use a box with f(x)
      - if f'(x) is available, use it to compute parallelogram and intersect it with the f(x) box
    -}
  toCanvasEnclosure x =
    let
      xMidPointValue = map boundsA $ evaluator.f xMidPoint

      xGradGrad = map boundsA $ evaluator.f'' x

      xGradient = case xGradGrad of
        Just (Tuple xGradGradLower xGradGradUpper)
          | xGradGradLower !>=! zero -> do
              xGradLeft <- evaluator.f' xLA
              xGradRight <- evaluator.f' xUA
              Just (Tuple (lowerA xGradLeft) (upperA xGradRight))
          | xGradGradUpper !<=! zero -> do
              xGradLeft <- evaluator.f' xLA
              xGradRight <- evaluator.f' xUA
              Just (Tuple (lowerA xGradRight) (upperA xGradLeft))
          | otherwise -> map boundsA $ evaluator.f' x
        _ -> map boundsA $ evaluator.f' x

      xValue = case xGradient of
        Just (Tuple xGradLower xGradUpper)
          | xGradLower !>=! zero -> do
              xLeft <- evaluator.f xLA
              xRight <- evaluator.f xUA
              Just (Tuple (lowerA xLeft) (upperA xRight))
          | xGradUpper !<=! zero -> do
              xLeft <- evaluator.f xLA
              xRight <- evaluator.f xUA
              Just (Tuple (lowerA xRight) (upperA xLeft))
          | otherwise -> map boundsA $ evaluator.f x
        _ -> map boundsA $ evaluator.f x

    in
      -- TODO: computer yLower yUpper by endpoints if gradient is positive or negative
      case xValue, xMidPointValue, xGradient of
        Just (Tuple yLower yUpper), Just (Tuple yMidLower yMidUpper), Just (Tuple lowerGradient upperGradient) | finite lowerGradient && finite upperGradient ->
          Just
            $ upperBoundary
            <> reverse lowerBoundary
            <> take 1 upperBoundary
          where
          yUpperRight = yMidUpper + ((enclosureWidth * upperGradient) / twoA)

          yUpperLeft = yMidUpper - ((enclosureWidth * lowerGradient) / twoA)

          yLowerRight = yMidLower + ((enclosureWidth * lowerGradient) / twoA)

          yLowerLeft = yMidLower - ((enclosureWidth * upperGradient) / twoA)

          upperBoundary =
            minHorizontalSlantedBoundary
              { xL: canvasXLower
              , xR: canvasXUpper
              , yU: toCanvasY yUpper
              , yUL: toCanvasY yUpperLeft
              , yUR: toCanvasY yUpperRight
              }

          lowerBoundary =
            map (\{ x: x_, y } -> { x: x_, y: (-y) })
              $ minHorizontalSlantedBoundary
                  { xL: canvasXLower
                  , xR: canvasXUpper
                  , yU: -(toCanvasY yLower)
                  , yUL: -(toCanvasY yLowerLeft)
                  , yUR: -(toCanvasY yLowerRight)
                  }

          minHorizontalSlantedBoundary params = aux $ unsafeSpy "params" params
            where
            aux { xL, xR, yU, yUL, yUR }
              -- box wins all round, use horizontal line:
              -- (In canvas coordinates Y origin is at the top, increasing Y goes donwwards!)
              | yU >= yUL && yU >= yUR 
                = [ { x: xL, y: yU }, { x: xR, y: yU } ]
              -- box loses all round, use slanted line:
              | yU <= yUL && yU <= yUR 
                = [ { x: xL, y: yUL }, { x: xR, y: yUR } ]
              -- box loses on the right, intersect both:
              | yU > yUL && yU <= yUR = [ { x: xL, y: yU }, { x: xI, y: yU }, { x: xR, y: yUR } ]
                where
                xI = xL + (yU - yUL) * (xR - xL) / (yUR - yUL)
              -- box loses on the left, intersect both:
              | otherwise = [ { x: xL, y: yUL }, { x: xI, y: yU }, { x: xR, y: yU } ]
                where
                xI = xL + (yU - yUL) * (xR - xL) / (yUR - yUL)
        Just (Tuple yLower yUpper), _, _ -> Just polygon
          where
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
