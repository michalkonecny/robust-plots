module Plot.RobustPlot where

import Prelude
import Data.Array (catMaybes, reverse, take)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Tuple (Tuple(..))
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative, ValueAndDerivative2, evaluateDerivative, evaluateDerivative2)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsA, boundsNumber, centreA, fromRationalPrec, isFinite, lowerA, toNumber, upperA)
import IntervalArith.Approx.NumOrder (absA, maxA, (!<=!), (!>=!))
import IntervalArith.Misc (Rational, rationalToNumber, two)
import Plot.Commands (Depth)
import Types (Polygon, Size, XYBounds)

drawRobustPlot :: Size -> XYBounds -> Expression -> Array (Tuple Depth Approx) -> String -> DrawCommand Unit
drawRobustPlot canvasSize bounds expression domainSegments label = drawCommands
  where
  segmentEnclosures = plotEnclosures canvasSize bounds domainSegments evaluateWithX evaluateWithX2

  drawCommands = drawPlot segmentEnclosures

  evaluateWithX x = value
    where
    variableMap = [ Tuple "x" { value: x, derivative: one } ]

    value = case evaluateDerivative variableMap expression of
      Left _ -> Nothing
      Right v -> Just v

  evaluateWithX2 x = value
    where
    variableMap = [ Tuple "x" { value: x, derivative: one, derivative2: zero } ]

    value = case evaluateDerivative2 variableMap expression of
      Left _ -> Nothing
      Right v -> Just v

plotEnclosures ::
  Size ->
  XYBounds ->
  Array (Tuple Depth Approx) ->
  (Approx -> Maybe (ValueAndDerivative Approx)) ->
  (Approx -> Maybe (ValueAndDerivative2 Approx)) ->
  Array (Maybe Polygon)
plotEnclosures canvasSize bounds domainSegments evaluator evaluator2 = segmentEnclosures
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

  toCanvasEnclosure :: (Tuple Depth Approx) -> Maybe Polygon
  {- overview:
      - try to compute f'(x)
        - if f''(x) is positive or negative, get f'(x) by endpoints
        - otherwise get f'(x) by midpoint and f''(x), or directly if f''(x) fails to compute
      - if f'(x) cannot be computed, use a box with f(x)
      - if f'(x) is available, use it to compute parallelogram and intersect it with the f(x) box
    -}
  toCanvasEnclosure (Tuple depth x) =
    let
      (Tuple canvasXLower canvasXUpper) = bimap toCanvasX toCanvasX $ boundsNumber x

      (Tuple xLA xUA) = boundsA x

      xMidPoint = centreA x

      enclosureWidth = xUA - xLA

      twoA = fromRationalPrec 50 two

      evaluatorX = evaluator2 x

      xMidPointValue = boundsA <$> (_.value) <$> evaluator xMidPoint

      xGradGrad = boundsA <$> (_.derivative2) <$> evaluatorX

      xGradient = case xGradGrad of
        Just (Tuple xGradGradLower xGradGradUpper)
          | xGradGradLower !>=! zero -> do
            xGradLeft <- (_.derivative) <$> evaluator xLA
            xGradRight <- (_.derivative) <$> evaluator xUA
            Just (Tuple (lowerA xGradLeft) (upperA xGradRight))
          | xGradGradUpper !<=! zero -> do
            xGradLeft <- (_.derivative) <$> evaluator xLA
            xGradRight <- (_.derivative) <$> evaluator xUA
            Just (Tuple (lowerA xGradRight) (upperA xGradLeft))
          | otherwise -> case boundsA <$> (_.derivative) <$> evaluator xMidPoint of
            Just (Tuple xGradientMidPointLower xGradientMidPointUpper)
              | isFinite xGradientMidPointLower && isFinite xGradientMidPointUpper ->
                let
                  xGradientVariation = upperA $ ((absA xGradGradLower) `maxA` (absA xGradGradUpper)) * enclosureWidth / twoA

                  xGradientUpper = xGradientMidPointUpper + xGradientVariation

                  xGradientLower = xGradientMidPointLower - xGradientVariation
                in
                  Just (Tuple xGradientLower xGradientUpper)
            _ -> map boundsA $ (_.derivative) <$> evaluatorX
        _ -> map boundsA $ (_.derivative) <$> evaluatorX

      xValue = case xGradient of
        Just (Tuple xGradLower xGradUpper)
          | xGradLower !>=! zero -> do
            xLeft <- (_.value) <$> evaluator xLA
            xRight <- (_.value) <$> evaluator xUA
            Just (Tuple (lowerA xLeft) (upperA xRight))
          | xGradUpper !<=! zero -> do
            xLeft <- (_.value) <$> evaluator xLA
            xRight <- (_.value) <$> evaluator xUA
            Just (Tuple (lowerA xRight) (upperA xLeft))
          | otherwise -> map boundsA $ (_.value) <$> evaluatorX
        _ -> map boundsA $ (_.value) <$> evaluatorX
    in
      case xValue, xMidPointValue, xGradient of
        Just (Tuple yLower yUpper), Just (Tuple yMidLower yMidUpper), Just (Tuple lowerGradient upperGradient)
          | isFinite lowerGradient && isFinite upperGradient ->
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
                , yU: toCanvasY { y: yUpper, roundDown: false }
                , yUL: toCanvasY { y: yUpperLeft, roundDown: false }
                , yUR: toCanvasY { y: yUpperRight, roundDown: false }
                }

            lowerBoundary =
              map (\{ x: x_, y } -> { x: x_, y: (-y) })
                $ minHorizontalSlantedBoundary
                    { xL: canvasXLower
                    , xR: canvasXUpper
                    , yU: -(toCanvasY { y: yLower, roundDown: true })
                    , yUL: -(toCanvasY { y: yLowerLeft, roundDown: true })
                    , yUR: -(toCanvasY { y: yLowerRight, roundDown: true })
                    }

            minHorizontalSlantedBoundary = aux
              where
              aux { xL, xR, yU, yUL, yUR }
                -- box wins all round, use horizontal line:
                -- (In canvas coordinates Y origin is at the top, increasing Y goes donwwards!)
                | yU >= yUL && yU >= yUR = [ { x: xL, y: yU }, { x: xR, y: yU } ]
                -- box loses all round, use slanted line:
                | yU <= yUL && yU <= yUR = [ { x: xL, y: yUL }, { x: xR, y: yUR } ]
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
          a = { x: canvasXLower, y: toCanvasY { y: yUpper, roundDown: false } }

          b = { x: canvasXLower, y: toCanvasY { y: yLower, roundDown: true } }

          c = { x: canvasXUpper, y: toCanvasY { y: yLower, roundDown: true } }

          d = { x: canvasXUpper, y: toCanvasY { y: yUpper, roundDown: false } }

          polygon = [ a, b, c, d, a ]
        _, _, _ -> Nothing

  toCanvasX :: Number -> Number
  toCanvasX x = ((x - xLowerBound) * canvasWidth) / rangeX

  toCanvasY { y: yApprox, roundDown } = safeCanvasY
    where
    y = toNumber yApprox

    canvasY = canvasHeight - (((y - yLowerBound) * canvasHeight) / rangeY)

    safeCanvasY = if Number.isFinite canvasY then canvasY else if roundDown then canvasHeight + one else -one

drawPlot :: Array (Maybe Polygon) -> DrawCommand Unit
drawPlot = (drawEnclosure true) <<< catMaybes
