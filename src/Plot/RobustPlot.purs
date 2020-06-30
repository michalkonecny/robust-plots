module Plot.RobustPlot where

import Prelude
import Data.Array (catMaybes, fromFoldable, tail, zipWith, (..))
import Data.Either (Either(..))
import Data.List (List, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (isFinite)
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Differentiator (differentiate)
import Expression.Evaluator (evaluate)
import Expression.Simplifier (simplify)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsA, fromRationalBoundsPrec, fromRationalPrec, toNumber, upperBound)
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
    Just approxValue -> case f xMidPoint of
      Nothing -> Nothing
      Just midApproxValue -> case f' x of
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
    x = fromRationalBoundsPrec 50 xLower xUpper

    xMidPoint = fromRationalPrec 50 $ (xLower + xUpper) / two

    enclosureWidth = fromRationalPrec 50 $ xUpper - xLower

    twoA = fromRationalPrec 50 $ two

    canvasXLower = toCanvasX xLower

    canvasXUpper = toCanvasX xUpper

  toCanvasX :: Rational -> Number
  toCanvasX x = rationalToNumber $ ((x - fullXBounds.lower) * canvasSize.width) / fullRangeX

  toCanvasY :: Approx -> Number
  toCanvasY yApprox = safeCanvasY
    where
    y = toNumber yApprox

    canvasY = canvasHeight - (((y - yLowerBound) * canvasHeight) / rangeY)

    safeCanvasY = if isFinite canvasY then canvasY else if canvasY < zero then canvasHeight + one else -one

drawPlot :: Array (Maybe Polygon) -> DrawCommand Unit
drawPlot = (drawEnclosure true) <<< catMaybes

segmentDomain :: Approx -> Approx -> (Approx -> Maybe Approx) -> Rational -> Rational -> Array Approx
segmentDomain accuracyTarget onePixel f'' l u = fromFoldable $ segementDomainF 0 l u
  where
  bisect :: Int -> Rational -> Rational -> Rational -> List Approx
  bisect depth lower mid upper = (segementDomainF (depth + one) lower mid) <> (segementDomainF (depth + one) mid upper)

  segementDomainF :: Int -> Rational -> Rational -> List Approx
  segementDomainF depth lower upper = segments
    where
    x = fromRationalBoundsPrec 50 lower upper

    mid = (lower + upper) / two

    segments =
      if depth < 5 then
        bisect depth lower mid upper
      else
        if depth >= 15 then
          singleton x
        else
          segmentBasedOnDerivative depth lower mid upper x

  segmentBasedOnDerivative :: Int -> Rational -> Rational -> Rational -> Approx -> List Approx
  segmentBasedOnDerivative depth lower mid upper x = case f'' x of
    Nothing -> singleton x
    Just deltaGradient ->
      if abs (upperBound deltaGradient) < upperBound onePixel then
        singleton x
      else
        bisect depth lower mid upper
