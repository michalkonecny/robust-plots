module Plot.RobustPlot where

import Prelude
import Data.Array (catMaybes, tail, zipWith, (..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (isFinite)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Effect.Exception.Unsafe (unsafeThrow)
import Expression.Differentiator (differentiate)
import Expression.Evaluator (evaluate)
import Expression.Simplifier (simplify)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsR, fromRationalBoundsPrec, fromRationalPrec)
import IntervalArith.Extended (Extended(..))
import IntervalArith.Misc (Rational, rationalToNumber, toRational, two)
import Types (Bounds, Polygon, Size, XYBounds)

drawRobustPlot :: Int -> Size -> Bounds -> XYBounds -> Expression -> String -> DrawCommand Unit
drawRobustPlot segmentCount canvasSize fullXBounds bounds expression label = drawCommands
  where
  f = evaluateWithX expression

  f' = (evaluateWithX <<< simplify <<< differentiate) expression

  segmentEnclosures = plotEnclosures segmentCount canvasSize fullXBounds bounds f f'

  drawCommands = drawPlot segmentEnclosures

evaluateWithX :: Expression -> Approx -> Maybe Approx
evaluateWithX expression x = value
  where
  variableMap = [ Tuple "x" x ]

  value = case evaluate variableMap expression of
    Left _ -> Nothing
    Right v -> Just v

plotEnclosures :: Int -> Size -> Bounds -> XYBounds -> (Approx -> Maybe Approx) -> (Approx -> Maybe Approx) -> Array (Maybe Polygon)
plotEnclosures segmentCount canvasSize fullXBounds bounds f f' = segmentEnclosures
  where
  rangeX = bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  fullRangeX = fullXBounds.upper - fullXBounds.lower

  domainBreakpoints = map (toRational >>> toDomainX) $ 0 .. segmentCount

  domainSegments = zipWith toRange domainBreakpoints (fromMaybe [] (tail domainBreakpoints))

  segmentWidth = rangeX / (toRational segmentCount)

  segmentEnclosures = map toCanvasEnclosure domainSegments

  yLowerBound = rationalToNumber bounds.yBounds.lower

  canvasHeight = rationalToNumber canvasSize.height

  halfRangeX = rangeX / (toRational 2)

  toRange :: Rational -> Rational -> Tuple Rational Rational
  toRange lower upper = Tuple lower upper

  toDomainX :: Rational -> Rational
  toDomainX segmentX = (segmentX * segmentWidth) + bounds.xBounds.lower

  toCanvasEnclosure :: Tuple Rational Rational -> Maybe Polygon
  toCanvasEnclosure (Tuple xLower xUpper) = case f x of
    Nothing -> Nothing
    Just approxValue -> case f xMid of
      Nothing -> Nothing
      Just midApproxValue -> case f' x of
        Nothing -> Nothing
        Just approxGradient -> Just polygon
          where
          (Tuple yLower yUpper) = boundsR approxValue

          (Tuple yMidLower yMidUpper) = boundsR midApproxValue

          (Tuple yLowerGradient yUpperGradient) = boundsR approxGradient

          a = { x: canvasXLower, y: toCanvasY $ yMidLower - ((w * yUpperGradient) / (pure two)) }

          b = { x: canvasXLower, y: toCanvasY $ yMidUpper - ((w * yLowerGradient) / (pure two)) }

          c = { x: canvasXUpper, y: toCanvasY $ yMidUpper + ((w * yUpperGradient) / (pure two)) }

          d = { x: canvasXUpper, y: toCanvasY $ yMidLower + ((w * yLowerGradient) / (pure two)) }

          polygon = [ a, b, c, d, a ]
    where
    x = fromRationalBoundsPrec 50 xLower xUpper

    xMid = fromRationalPrec 50 $ (xLower + xUpper) / two

    w = pure $ (xUpper - xLower) / two

    canvasXLower = toCanvasX xLower

    canvasXUpper = toCanvasX xUpper

  toCanvasX :: Rational -> Number
  toCanvasX x = rationalToNumber $ ((x - fullXBounds.lower) * canvasSize.width) / fullRangeX

  toCanvasY :: Extended Rational -> Number
  toCanvasY (Finite yRational) = safeCanvasY
    where
    y = rationalToNumber yRational

    canvasY = canvasHeight - (((y - yLowerBound) * canvasHeight) / rangeY)

    safeCanvasY = if isFinite canvasY then canvasY else if canvasY < zero then canvasHeight + one else -one

  toCanvasY PosInf = -one

  toCanvasY NegInf = canvasHeight + one

drawPlot :: Array (Maybe Polygon) -> DrawCommand Unit
drawPlot = (drawEnclosure true) <<< catMaybes
