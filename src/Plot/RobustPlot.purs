module Plot.RobustPlot where

import Prelude
import Data.Array (catMaybes, concat, head, length, reverse, take)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Number as Number
import Data.Tuple (Tuple(..), snd)
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative, ValueAndDerivative2, evaluateDerivative, evaluateDerivative2)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsA, boundsNumber, centreA, isFinite, lowerA, mBound, setMB, toNumber, unionA, upperA)
import IntervalArith.Approx.NumOrder (absA, maxA, minA, (!<=!), (!>=!))
import IntervalArith.Misc (rationalToNumber, two)
import Misc.Debug (unsafeLog)
import Partial.Unsafe (unsafePartial)
import Plot.Commands (Depth)
import Plot.Segments (maxDepth)
import Types (Polygon, Size, XYBounds)

shouldLogSubsegments :: Boolean
shouldLogSubsegments = false

shouldLogEnclosures :: Boolean
shouldLogEnclosures = false

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
plotEnclosures { canvasSize, bounds, domainSegments, accuracyTarget, evaluator, evaluator2 } =
  unsafeLog
    ("plotEnclosures: sum (map length segmentEnclosures) = " <> show (sum (map length segmentEnclosures)))
    segmentEnclosures
  where
  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  rangeX = rationalToNumber $ bounds.xBounds.upper - bounds.xBounds.lower

  yLowerBound = rationalToNumber bounds.yBounds.lower

  xLowerBound = rationalToNumber bounds.xBounds.lower

  canvasHeight = rationalToNumber canvasSize.height

  canvasWidth = rationalToNumber canvasSize.width

  extraPrecision = raiseUntilGoodOrNotImproving 5 Nothing 0 0
    where
    x = snd $ unsafePartial $ fromJust $ head domainSegments

    xPrecision = mBound x

    raiseUntilGoodOrNotImproving attemptsLeft maybePreviousAccuracy p pToTry
      | attemptsLeft <= 0 = p
      | otherwise = case toCanvasEnclosure (setMB (xPrecision + pToTry) x), maybePreviousAccuracy of
        Just (Tuple _ a), _
          | a <= accuracyTarget -> pToTry -- pToTry met target accuracy
        Just (Tuple _ a), Just previousAccuracy
          | a > previousAccuracy -> p -- pToTry made it worse, revert to p
          | a <= 0.75 * previousAccuracy -> raiseUntilGoodOrNotImproving (attemptsLeft - 1) (Just a) pToTry (pToTry + 10)
          | otherwise -> raiseUntilGoodOrNotImproving (attemptsLeft - 1) (Just a) p (pToTry + 10)
        Just (Tuple _ a), _ -> raiseUntilGoodOrNotImproving (attemptsLeft - 1) (Just a) pToTry (pToTry + 10)
        _, _ -> raiseUntilGoodOrNotImproving (attemptsLeft - 1) maybePreviousAccuracy p (pToTry + 10)

  addExtraPrecision (Tuple depth x) = Tuple depth (setMB (mBound x + extraPrecision) x)

  segmentEnclosures = map (toCanvasEnclosures <<< addExtraPrecision) domainSegments

  toCanvasEnclosures :: (Tuple Depth Approx) -> Array (Maybe Polygon)
  toCanvasEnclosures (Tuple depth x) = case toCanvasEnclosure x of
    Just (Tuple polygon accuracy)
      | debugLog accuracy $ accuracy <= accuracyTarget || depth >= maxDepth -> [ Just polygon ]
    _
      | depth >= maxDepth -> [ Nothing ]
    _ -> bisect
      where
      setHigherPrecision = setMB (2 + (mBound x))

      bisect = enclosuresLeft <> enclosuresRight
        where
        (Tuple xL xU) = boundsA x

        xLeft = setHigherPrecision $ xL `unionA` xM

        xRight = setHigherPrecision $ xM `unionA` xU

        xM = (xL + xU) / two

        enclosuresLeft = toCanvasEnclosures (Tuple (depth + 1) xLeft)

        enclosuresRight = toCanvasEnclosures (Tuple (depth + 1) xRight)
    where
    debugLog accuracy =
      logSubsegments
        $ "x = "
        <> show (boundsNumber x)
        <> ", depth = "
        <> show depth
        <> ", mb = "
        <> show (mBound x)
        <> ", accuracy = "
        <> show accuracy
        <> ", accuracyTarget = "
        <> show accuracyTarget
        <> if accuracy <= accuracyTarget then
            ""
          else
            ", INSUFFICIENT ACCURACY "

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

      xValueDirect = map boundsA $ (_.value) <$> evaluatorX

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
          | otherwise -> xValueDirect
        _ -> xValueDirect

      showEATuple t = show (bimap toNumber toNumber <$> t)

      logEnclosureInputValues =
        logEnclosures
          $ "x = "
          <> show (boundsNumber x)
          <> ", xValueDirect = "
          <> showEATuple xValueDirect
          <> ", xValue = "
          <> showEATuple xValue
          <> ", xGradient = "
          <> showEATuple xGradient
          <> ", xGradGrad = "
          <> showEATuple xGradGrad
    in
      case logEnclosureInputValues xValue, xMidPointValue, xGradient of
        Just (Tuple yLower yUpper), Just (Tuple yMidLower yMidUpper), Just (Tuple lowerGradient upperGradient)
          | isFinite lowerGradient && isFinite upperGradient -> Just (Tuple polygon accuracy)
            where
            polygon = upperBoundary <> reverse lowerBoundary <> take 1 upperBoundary

            accuracy = snd $ boundsNumber accuracyA

            accuracyA = maxA (minA enclosureWidth enclosureParallelogramWidth) (minA enclosureBoxHeight enclosureParallelogramHeight)
              where
              enclosureBoxHeight = (yUpper - yLower) / (one + (maxA (absA upperGradient) (absA lowerGradient)))

              enclosureParallelogramHeight = enclosureWidthHalf * (upperGradient - lowerGradient) + (yMidUpper - yMidLower)

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

logSubsegments :: forall a. String -> a -> a
logSubsegments = logSomething shouldLogSubsegments

logEnclosures :: forall a. String -> a -> a
logEnclosures = logSomething shouldLogEnclosures

logSomething :: forall a. Boolean -> String -> a -> a
logSomething shouldLogSomething
  | shouldLogSomething = unsafeLog
  | otherwise = \_ a -> a
