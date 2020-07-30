module Plot.RobustParametricPlot where

import Prelude
import Data.Array (catMaybes, concat)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Tuple (Tuple(..), snd)
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Error (expectToMaybe)
import Expression.Evaluate.AutomaticDifferentiator (evaluateDerivative, evaluateDerivative2)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsA, boundsNumber, centreA, isFinite, lowerA, mBound, setMB, toNumber, unionA, upperA)
import IntervalArith.Approx.NumOrder (absA, maxA, (!<=!), (!>=!))
import IntervalArith.Misc (rationalToNumber, two)
import Plot.Commands (Depth)
import Plot.FunctionSegments (maxDepth)
import Plot.Parametric (ValueAndDerivativePair, ValueAndDerivativePair2, evaluateParametric, evaluateParametric2, toX, toY)
import Types (Polygon, Size, XYBounds)

shouldLogSubsegments :: Boolean
shouldLogSubsegments = false

shouldLogEnclosures :: Boolean
shouldLogEnclosures = false

drawRobustParametricPlot :: Size -> XYBounds -> Expression -> Expression -> Array (Tuple Depth Approx) -> Number -> DrawCommand Unit
drawRobustParametricPlot canvasSize bounds xExpression yExpression domainSegments accuracyTarget = drawCommands
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
plotEnclosures { canvasSize, bounds, domainSegments, accuracyTarget, evaluator, evaluator2 } = segmentEnclosures
  where
  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  rangeX = rationalToNumber $ bounds.xBounds.upper - bounds.xBounds.lower

  yLowerBound = rationalToNumber bounds.yBounds.lower

  xLowerBound = rationalToNumber bounds.xBounds.lower

  canvasHeight = rationalToNumber canvasSize.height

  canvasWidth = rationalToNumber canvasSize.width

  segmentEnclosures = map toCanvasEnclosures domainSegments

  toCanvasEnclosures :: (Tuple Depth Approx) -> Array (Maybe Polygon)
  toCanvasEnclosures (Tuple depth t) = case toCanvasEnclosure toX t, toCanvasEnclosure toY t of
    Just (Tuple polygonX accuracyX), Just (Tuple polygonY accuracyY)
      | (accuracyX <= accuracyTarget && accuracyY <= accuracyTarget) || depth >= maxDepth -> [ combine polygonX polygonY ]
    _, _
      | depth >= maxDepth -> [ Nothing ]
      | otherwise -> bisect
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

  combine :: Tuple Approx Approx -> Tuple Approx Approx -> Maybe Polygon
  combine (Tuple xLower xUpper) (Tuple yLower yUpper) = Just [ a, b, c, d, a ]
    where
    a = { x: toCanvasX xLower, y: toCanvasY { y: yUpper, roundDown: false } }

    b = { x: toCanvasX xLower, y: toCanvasY { y: yLower, roundDown: true } }

    c = { x: toCanvasX xUpper, y: toCanvasY { y: yLower, roundDown: true } }

    d = { x: toCanvasX xUpper, y: toCanvasY { y: yUpper, roundDown: false } }

  toCanvasEnclosure :: (forall a v r. { x :: v a, y :: v a | r } -> v a) -> Approx -> Maybe (Tuple (Tuple Approx Approx) Number)
  {- overview:
      - try to compute f'(t)
        - if f''(t) is positive or negative, get f'(t) by endpoints
        - otherwise get f'(t) by midpoint and f''(t), or directly if f''(t) fails to compute
      - if f'(t) cannot be computed, use a box with f(t)
      - if f'(t) is available, use it to compute parallelogram and intersect it with the f(t) box
    -}
  toCanvasEnclosure parametricToFunction t =
    let
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
    in
      case value of
        Just v@(Tuple yLower yUpper) -> Just (Tuple v accuracy)
          where
          accuracy = snd $ boundsNumber $ yUpper - yLower
        _ -> Nothing

  toCanvasX :: Approx -> Number
  toCanvasX xApprox = ((x - xLowerBound) * canvasWidth) / rangeX
    where
    x = toNumber xApprox

  toCanvasY { y: yApprox, roundDown } = safeCanvasY
    where
    y = toNumber yApprox

    canvasY = canvasHeight - (((y - yLowerBound) * canvasHeight) / rangeY)

    safeCanvasY = if Number.isFinite canvasY then canvasY else if roundDown then canvasHeight + one else -one

drawPlot :: Array (Maybe Polygon) -> DrawCommand Unit
drawPlot = (drawEnclosure true) <<< catMaybes
