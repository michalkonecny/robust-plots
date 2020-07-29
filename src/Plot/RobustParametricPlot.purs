module Plot.RobustParametricPlot where

import Prelude
import Data.Array (catMaybes, concat, head, length, reverse, take)
import Data.Bifunctor (bimap)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Number as Number
import Data.Tuple (Tuple(..), snd)
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Error (expectToMaybe)
import Expression.Evaluate.AutomaticDifferentiator (evaluateDerivative, evaluateDerivative2)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsA, boundsNumber, centreA, isFinite, lowerA, mBound, setMB, toNumber, unionA, upperA)
import IntervalArith.Approx.NumOrder (absA, maxA, minA, (!<=!), (!>=!))
import IntervalArith.Misc (rationalToNumber, two)
import Misc.Debug (unsafeLog)
import Partial.Unsafe (unsafePartial)
import Plot.Commands (Depth)
import Plot.FunctionSegments (maxDepth)
import Plot.Parametric (ValueAndDerivativePair, ValueAndDerivativePair2, evaluateParametric, evaluateParametric2, toX)
import Types (Polygon, Size, XYBounds, Bounds)

shouldLogSubsegments :: Boolean
shouldLogSubsegments = false

shouldLogEnclosures :: Boolean
shouldLogEnclosures = false

drawRobustParametricPlot :: Size -> XYBounds -> Bounds -> Expression -> Expression -> Array (Tuple Depth Approx) -> Number -> DrawCommand Unit
drawRobustParametricPlot canvasSize bounds domain xExpression yExpression domainSegments accuracyTarget = drawCommands
  where
  segmentEnclosures = plotEnclosures { canvasSize, bounds, domainSegments, accuracyTarget, evaluator: evaluateWithT, evaluator2: evaluateWithT2 }

  drawCommands = drawPlot $ concat segmentEnclosures

  evaluateWithT :: Approx -> Maybe (ValueAndDerivativePair Approx)
  evaluateWithT t = evaluateParametric evaluator xExpression yExpression
    where
    evaluator = expectToMaybe <<< evaluateDerivative variableMap

    variableMap = [ Tuple "t" { value: t, derivative: one } ]

  evaluateWithT2 :: Approx -> Maybe (ValueAndDerivativePair2 Approx)
  evaluateWithT2 t = evaluateParametric2 evaluator2 xExpression yExpression
    where
    evaluator2 = expectToMaybe <<< evaluateDerivative2 variableMap2

    variableMap2 = [ Tuple "t" { value: t, derivative: one, derivative2: zero } ]

plotEnclosures ::
  { bounds :: XYBounds
  , canvasSize :: Size
  , domainSegments :: Array (Tuple Int Approx)
  , accuracyTarget :: Number
  , evaluator :: Approx -> Maybe (ValueAndDerivativePair Approx)
  , evaluator2 :: Approx -> Maybe (ValueAndDerivativePair2 Approx)
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

  extraPrecision :: Int
  extraPrecision = raiseUntilGoodOrNotImproving 5 Nothing 0 0
    where
    t = snd $ unsafePartial $ fromJust $ head domainSegments

    tPrecision = mBound t

    raiseUntilGoodOrNotImproving attemptsLeft maybePreviousAccuracy p pToTry
      | attemptsLeft <= 0 = p
      | otherwise = case toCanvasEnclosure toX (setMB (tPrecision + pToTry) t), maybePreviousAccuracy of
        Just (Tuple _ a), _
          | a <= accuracyTarget -> pToTry -- pToTry met target accuracy
        Just (Tuple _ a), Just previousAccuracy
          | a > previousAccuracy -> p -- pToTry made it worse, revert to p
          | a <= 0.75 * previousAccuracy -> raiseUntilGoodOrNotImproving (attemptsLeft - 1) (Just a) pToTry (pToTry + 10)
          | otherwise -> raiseUntilGoodOrNotImproving (attemptsLeft - 1) (Just a) p (pToTry + 10)
        Just (Tuple _ a), _ -> raiseUntilGoodOrNotImproving (attemptsLeft - 1) (Just a) pToTry (pToTry + 10)
        _, _ -> raiseUntilGoodOrNotImproving (attemptsLeft - 1) maybePreviousAccuracy p (pToTry + 10)

  addExtraPrecision (Tuple depth t) = Tuple depth (setMB (mBound t + extraPrecision) t)

  segmentEnclosures = map (toCanvasEnclosures <<< addExtraPrecision) domainSegments

  toCanvasEnclosures :: (Tuple Depth Approx) -> Array (Maybe Polygon)
  toCanvasEnclosures (Tuple depth t) = case toCanvasEnclosure toX t of
    Just (Tuple polygon accuracy)
      | debugLog accuracy $ accuracy <= accuracyTarget || depth >= maxDepth -> [ Just polygon ]
    _
      | depth >= maxDepth -> [ Nothing ]
    _ -> bisect
      where
      setHigherPrecision = setMB (2 + (mBound t))

      bisect = enclosuresLeft <> enclosuresRight
        where
        (Tuple tL tU) = boundsA t

        left = setHigherPrecision $ tL `unionA` tM

        right = setHigherPrecision $ tM `unionA` tU

        tM = (tL + tU) / two

        enclosuresLeft = toCanvasEnclosures (Tuple (depth + 1) left)

        enclosuresRight = toCanvasEnclosures (Tuple (depth + 1) right)
    where
    debugLog accuracy =
      logSubsegments
        $ "t = "
        <> show (boundsNumber t)
        <> ", depth = "
        <> show depth
        <> ", mb = "
        <> show (mBound t)
        <> ", accuracy = "
        <> show accuracy
        <> ", accuracyTarget = "
        <> show accuracyTarget
        <> if accuracy <= accuracyTarget then
            ""
          else
            ", INSUFFICIENT ACCURACY "

  toCanvasEnclosure :: (forall a v r. { x :: v a, y :: v a | r } -> v a) -> Approx -> Maybe (Tuple Polygon Number)
  {- overview:
      - try to compute f'(t)
        - if f''(t) is positive or negative, get f'(t) by endpoints
        - otherwise get f'(t) by midpoint and f''(t), or directly if f''(t) fails to compute
      - if f'(t) cannot be computed, use a box with f(t)
      - if f'(t) is available, use it to compute parallelogram and intersect it with the f(t) box
    -}
  toCanvasEnclosure parametricToFunction t =
    let
      (Tuple canvasXLower canvasXUpper) = bimap toCanvasX toCanvasX $ boundsNumber t

      (Tuple tLA tUA) = boundsA t

      midPoint = centreA t

      enclosureWidth = tUA - tLA

      enclosureWidthHalf = enclosureWidth / two

      evaluatorT = evaluator2 t

      midPointValue = boundsA <$> (_.value) <$> parametricToFunction <$> evaluator midPoint

      gradGrad = boundsA <$> (_.derivative2) <$> parametricToFunction <$> evaluatorT

      gradient = case gradGrad of
        Just (Tuple gradGradLower gradGradUpper)
          | gradGradLower !>=! zero -> do
            gradLeft <- (_.derivative) <$> parametricToFunction <$> evaluator tLA
            gradRight <- (_.derivative) <$> parametricToFunction <$> evaluator tUA
            Just (Tuple (lowerA gradLeft) (upperA gradRight))
          | gradGradUpper !<=! zero -> do
            gradLeft <- (_.derivative) <$> parametricToFunction <$> evaluator tLA
            gradRight <- (_.derivative) <$> parametricToFunction <$> evaluator tUA
            Just (Tuple (lowerA gradRight) (upperA gradLeft))
          | isFinite gradGradLower && isFinite gradGradUpper -> case boundsA <$> (_.derivative) <$> parametricToFunction <$> evaluator midPoint of
            Just (Tuple gradientMidPointLower gradientMidPointUpper)
              | isFinite gradientMidPointLower && isFinite gradientMidPointUpper ->
                let
                  gradientVariation = upperA $ ((absA gradGradLower) `maxA` (absA gradGradUpper)) * enclosureWidthHalf

                  gradientUpper = gradientMidPointUpper + gradientVariation

                  gradientLower = gradientMidPointLower - gradientVariation
                in
                  Just (Tuple gradientLower gradientUpper)
            _ -> map boundsA $ (_.derivative) <$> parametricToFunction <$> evaluatorT
        _ -> map boundsA $ (_.derivative) <$> parametricToFunction <$> evaluatorT

      valueDirect = map boundsA $ (_.value) <$> parametricToFunction <$> evaluatorT

      value = case gradient of
        Just (Tuple gradLower gradUpper)
          | gradLower !>=! zero -> do
            left <- (_.value) <$> parametricToFunction <$> evaluator tLA
            right <- (_.value) <$> parametricToFunction <$> evaluator tUA
            Just (Tuple (lowerA left) (upperA right))
          | gradUpper !<=! zero -> do
            left <- (_.value) <$> parametricToFunction <$> evaluator tLA
            right <- (_.value) <$> parametricToFunction <$> evaluator tUA
            Just (Tuple (lowerA right) (upperA left))
          | otherwise -> valueDirect
        _ -> valueDirect

      showEATuple a = show (bimap toNumber toNumber <$> a)

      logEnclosureInputValues =
        logEnclosures
          $ "t = "
          <> show (boundsNumber t)
          <> ", valueDirect = "
          <> showEATuple valueDirect
          <> ", value = "
          <> showEATuple value
          <> ", gradient = "
          <> showEATuple gradient
          <> ", gradGrad = "
          <> showEATuple gradGrad
    in
      case logEnclosureInputValues value, midPointValue, gradient of
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
