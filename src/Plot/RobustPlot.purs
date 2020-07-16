module Plot.RobustPlot where

import Prelude
import Data.Either (Either(..))
import Data.Array (catMaybes, reverse, take)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Tuple (Tuple(..))
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative2, evaluateDerivative2)
import Expression.Syntax (Expression)
import Expression.VariableMap (VariableMap)
import IntervalArith.Approx (Approx, boundsA, boundsNumber, centreA, isFinite, fromRationalPrec, lowerA, toNumber, upperA)
import IntervalArith.Approx.ExpLog (eA)
import IntervalArith.Approx.Pi (piA)
import IntervalArith.Approx.NumOrder ((!<=!), (!>=!))
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
  {- overview:
      - try to compute f'(x)
        - if f''(x) is positive or negative, get f'(x) by endpoints
        - otherwise get f'(x) by midpoint and f''(x), or directly if f''(x) fails to compute
      - if f'(x) cannot be computed, use a box with f(x)
      - if f'(x) is available, use it to compute parallelogram and intersect it with the f(x) box
    -}
  toCanvasEnclosure x =
    let
      fx = evaluator x

      fxLA = evaluator xLA

      fxUA = evaluator xUA

      fxMidPoint = evaluator xMidPoint

      xMidPointValue = boundsA <$> (fxMidPoint <#> (_.value))

      xGradGrad = boundsA <$> (fx <#> (_.derivative2))

      xGradient = case xGradGrad of
        Just (Tuple xGradGradLower xGradGradUpper)
          | xGradGradLower !>=! zero -> do
            xGradLeft <- (_.derivative) <$> fxLA
            xGradRight <- (_.derivative) <$> fxUA
            Just (Tuple (lowerA xGradLeft) (upperA xGradRight))
          | xGradGradUpper !<=! zero -> do
            xGradLeft <- (_.derivative) <$> fxLA
            xGradRight <- (_.derivative) <$> fxUA
            Just (Tuple (lowerA xGradRight) (upperA xGradLeft))
          | otherwise -> map boundsA $ (_.derivative) <$> fx
        _ -> map boundsA $ (_.derivative) <$> fx

      xValue = case xGradient of
        Just (Tuple xGradLower xGradUpper)
          | xGradLower !>=! zero -> do
            xLeft <- (_.value) <$> fxLA
            xRight <- (_.value) <$> fxUA
            Just (Tuple (lowerA xLeft) (upperA xRight))
          | xGradUpper !<=! zero -> do
            xLeft <- (_.value) <$> fxLA
            xRight <- (_.value) <$> fxUA
            Just (Tuple (lowerA xRight) (upperA xLeft))
          | otherwise -> map boundsA $ (_.value) <$> fx
        _ -> map boundsA $ (_.value) <$> fx
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

    safeCanvasY = if Number.isFinite canvasY then canvasY else if canvasY < zero then canvasHeight + one else -one

drawPlot :: Array (Maybe Polygon) -> DrawCommand Unit
drawPlot = (drawEnclosure true) <<< catMaybes
