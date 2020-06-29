module Plot.RobustPlot where

import Prelude
import Data.Array (catMaybes, tail, zipWith, (..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (isFinite)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Evaluator (evaluate)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsNumber, fromRationalBoundsPrec)
import IntervalArith.Misc (Rational, rationalToNumber, toRational)
import Types (Bounds, Polygon, Size, XYBounds)

drawRobustPlot :: Int -> Size -> Bounds -> XYBounds -> Expression -> String -> DrawCommand Unit
drawRobustPlot segmentCount canvasSize fullXBounds bounds expression label = drawCommands
  where
  f = evaluateWithX expression

  segmentEnclosures = plotEnclosures segmentCount canvasSize fullXBounds bounds f

  drawCommands = drawPlot segmentEnclosures

evaluateWithX :: Expression -> Approx -> Maybe Approx
evaluateWithX expression x = value
  where
  variableMap = [ Tuple "x" x ]

  value = case evaluate variableMap expression of
    Left _ -> Nothing
    Right v -> Just v

plotEnclosures :: Int -> Size -> Bounds -> XYBounds -> (Approx -> Maybe Approx) -> Array (Maybe Polygon)
plotEnclosures segmentCount canvasSize fullXBounds bounds f = segmentEnclosures
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

  applyExpression :: Rational -> Rational -> Maybe (Tuple Number Number)
  applyExpression xLower xUpper = case f $ fromRationalBoundsPrec 50 xLower xUpper of
    Nothing -> Nothing
    Just approxValue -> Just $ boundsNumber approxValue

  toCanvasEnclosure :: Tuple Rational Rational -> Maybe Polygon
  toCanvasEnclosure (Tuple xLower xUpper) = case applyExpression xLower xUpper of
    Nothing -> Nothing
    Just (Tuple yLower yUpper) -> Just polygon
      where
      canvasXLower = toCanvasX xLower

      canvasXUpper = toCanvasX xUpper

      canvasYLower = toCanvasY yLower

      canvasYUpper = toCanvasY yUpper

      a = { x: canvasXLower, y: canvasYUpper }

      b = { x: canvasXUpper, y: canvasYUpper }

      c = { x: canvasXUpper, y: canvasYLower }

      d = { x: canvasXLower, y: canvasYLower }

      polygon = [ a, b, c, d, a ]

  toCanvasX :: Rational -> Number
  toCanvasX x = rationalToNumber $ ((x - fullXBounds.lower) * canvasSize.width) / fullRangeX

  toCanvasY :: Number -> Number
  toCanvasY y = safeCanvasY
    where
    canvasY = canvasHeight - (((y - yLowerBound) * canvasHeight) / rangeY)

    safeCanvasY = if isFinite canvasY then canvasY else if canvasY < zero then canvasHeight + one else -one

drawPlot :: Array (Maybe Polygon) -> DrawCommand Unit
drawPlot = (drawEnclosure true) <<< catMaybes
