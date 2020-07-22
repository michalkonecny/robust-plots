module Plot.RobustPlot where

import Prelude
import Data.Array (catMaybes, concat, reverse, take)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Tuple (Tuple(..), snd)
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative, ValueAndDerivative2, evaluateDerivative, evaluateDerivative2)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsA, boundsNumber, centreA, isFinite, lowerA, toNumber, unionA, upperA)
import IntervalArith.Approx.NumOrder (absA, maxA, minA, (!<=!), (!>=!))
import IntervalArith.Misc (Rational, rationalToNumber, two)
import Misc.Debug (unsafeLog)
import Plot.Commands (Depth)
import Plot.Segments (maxDepth)
import Types (Polygon, Size, XYBounds)

shouldLog :: Boolean
shouldLog = false

log :: forall a. String -> a -> a
log
  | shouldLog = unsafeLog
  | otherwise = \_ a -> a

drawRobustPlot :: Size -> XYBounds -> Expression -> Array (Tuple Depth Approx) -> Number -> String -> DrawCommand Unit
drawRobustPlot canvasSize bounds expression domainSegments accuracyTarget label = drawCommands
  where
  segmentEnclosures = plotEnclosures { canvasSize, bounds, domainSegments, accuracyTarget, evaluator: evaluateWithX, evaluator2: evaluateWithX2 }

  drawCommands = drawPlot $ concat segmentEnclosures

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
  { bounds :: XYBounds
  , canvasSize :: Size
  , domainSegments :: Array (Tuple Int Approx)
  , accuracyTarget :: Number
  , evaluator :: Approx -> Maybe (ValueAndDerivative Approx)
  , evaluator2 :: Approx -> Maybe (ValueAndDerivative2 Approx)
  } ->
  Array (Array (Maybe Polygon))
plotEnclosures { canvasSize, bounds, domainSegments, accuracyTarget, evaluator, evaluator2 } = segmentEnclosures
  where
  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  rangeX = rationalToNumber $ bounds.xBounds.upper - bounds.xBounds.lower

  segmentEnclosures = map toCanvasEnclosures domainSegments

  yLowerBound = rationalToNumber bounds.yBounds.lower

  xLowerBound = rationalToNumber bounds.xBounds.lower

  canvasHeight = rationalToNumber canvasSize.height

  canvasWidth = rationalToNumber canvasSize.width

  toRange :: Rational -> Rational -> Tuple Rational Rational
  toRange lower upper = Tuple lower upper

  toCanvasEnclosures :: (Tuple Depth Approx) -> Array (Maybe Polygon)
  toCanvasEnclosures (Tuple depth x) = case toCanvasEnclosure x of
    Just (Tuple polygon accuracy)
      | debugLog accuracy $ accuracy <= accuracyTarget || depth >= maxDepth -> [ Just polygon ]
    _
      | depth >= maxDepth -> [ Nothing ]
    _ -> bisect
      where
      bisect = enclosuresLeft <> enclosuresRight
        where
        (Tuple xL xU) = boundsA x

        xLeft = xL `unionA` xM

        xRight = xM `unionA` xU

        xM = (xL + xU) / two

        enclosuresLeft = toCanvasEnclosures (Tuple (depth + 1) xLeft)

        enclosuresRight = toCanvasEnclosures (Tuple (depth + 1) xRight)
    where
    debugLog accuracy =
      log
        $ "x = "
        <> show (boundsNumber x)
        <> ", depth = "
        <> show depth
        <> if accuracy <= accuracyTarget then
            ""
          else
            ", INSUFFICIENT ACCURACY "
              <> ", accuracy = "
              <> show accuracy
              <> ", accuracyTarget = "
              <> show accuracyTarget

  toCanvasEnclosure :: Approx -> Maybe (Tuple Polygon Number)
  {- overview:
      - try to compute f'(x)
        - if f''(x) is positive or negative, get f'(x) by endpoints
        - otherwise get f'(x) by midpoint and f''(x), or directly if f''(x) fails to compute
      - if f'(x) cannot be computed, use a box with f(x)
      - if f'(x) is available, use it to compute parallelogram and intersect it with the f(x) box
    -}
  toCanvasEnclosure x =
    let
      (Tuple canvasXLower canvasXUpper) = bimap toCanvasX toCanvasX $ boundsNumber x

      (Tuple xLA xUA) = boundsA x

      xMidPoint = centreA x

      enclosureWidth = xUA - xLA

      enclosureWidthHalf = enclosureWidth / two

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
          | isFinite xGradGradLower && isFinite xGradGradUpper -> case boundsA <$> (_.derivative) <$> evaluator xMidPoint of
            Just (Tuple xGradientMidPointLower xGradientMidPointUpper)
              | isFinite xGradientMidPointLower && isFinite xGradientMidPointUpper ->
                let
                  xGradientVariation = upperA $ ((absA xGradGradLower) `maxA` (absA xGradGradUpper)) * enclosureWidthHalf

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
          | isFinite lowerGradient && isFinite upperGradient -> Just (Tuple polygon accuracy)
            where
            polygon = upperBoundary <> reverse lowerBoundary <> take 1 upperBoundary

            accuracy = snd $ boundsNumber accuracyA

            accuracyA = minA (minA enclosureWidth enclosureParallelogramWidth) (minA enclosureBoxHeight enclosureParallelogramHeight)
              where
              enclosureBoxHeight = yUpper - yLower

              enclosureParallelogramHeight = enclosureWidthHalf * (upperGradient - lowerGradient)

              enclosureParallelogramWidth = enclosureParallelogramHeight / (absA upperGradient)

            yUpperRight = yMidUpper + (enclosureWidthHalf * upperGradient)

            yUpperLeft = yMidUpper - (enclosureWidthHalf * lowerGradient)

            yLowerRight = yMidLower + (enclosureWidthHalf * lowerGradient)

            yLowerLeft = yMidLower - (enclosureWidthHalf * upperGradient)

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
        Just (Tuple yLower yUpper), _, _ -> Just (Tuple polygon accuracy)
          where
          a = { x: canvasXLower, y: toCanvasY { y: yUpper, roundDown: false } }

          b = { x: canvasXLower, y: toCanvasY { y: yLower, roundDown: true } }

          c = { x: canvasXUpper, y: toCanvasY { y: yLower, roundDown: true } }

          d = { x: canvasXUpper, y: toCanvasY { y: yUpper, roundDown: false } }

          polygon = [ a, b, c, d, a ]

          accuracy = snd $ boundsNumber $ yUpper - yLower
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
