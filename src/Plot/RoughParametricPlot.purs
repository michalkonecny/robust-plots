module Plot.RoughParametricPlot where

import Prelude
<<<<<<< HEAD
import Data.Array (tail, zipWith, (..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
=======
import Data.Array (concat, mapMaybe, tail, zipWith, (..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..), fst, snd)
>>>>>>> parametric-robust
import Draw.Actions (drawPlotLine)
import Draw.Commands (DrawCommand)
import Expression.Error (expectToMaybe)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative2, evaluateDerivative2)
import Expression.Syntax (Expression)
import IntervalArith.Misc (rationalToNumber)
<<<<<<< HEAD
import Math (e, pi)
import Types (Position, Size, XYBounds, Bounds)

=======
import Math (e, pi, sqrt)
import Types (Position, Size, XYBounds, Bounds)

maxDistanceBetweenPoints :: Number
maxDistanceBetweenPoints = 10.0

numberOfDomainPoints :: Number
numberOfDomainPoints = 2.0

>>>>>>> parametric-robust
drawRoughParametricPlot :: Size -> XYBounds -> Bounds -> Expression -> Expression -> DrawCommand Unit
drawRoughParametricPlot canvasSize bounds domain xExpression yExpression = drawCommands
  where
  f = evaluateWithT xExpression yExpression

<<<<<<< HEAD
  points = plotPoints canvasSize bounds domain f
=======
  points = plotPoints canvasSize bounds (toNumberBounds domain) f
>>>>>>> parametric-robust

  drawCommands = drawPlot points

type ValueAndDerivativePair2 a
  = { x :: ValueAndDerivative2 a, y :: ValueAndDerivative2 a }

evaluateWithT :: Expression -> Expression -> Number -> Maybe (ValueAndDerivativePair2 Number)
evaluateWithT xExpression yExpression t = result
  where
  variableMap =
    [ Tuple "t" { value: t, derivative: 1.0, derivative2: 0.0 }
    , Tuple "e" { value: e, derivative: 0.0, derivative2: 0.0 }
    , Tuple "pi" { value: pi, derivative: 0.0, derivative2: 0.0 }
    ]

  evaluator = expectToMaybe <<< evaluateDerivative2 variableMap

  result = case evaluator xExpression, evaluator yExpression of
    Just x, Just y -> Just { x, y }
    _, _ -> Nothing

<<<<<<< HEAD
plotPoints :: Size -> XYBounds -> Bounds -> (Number -> Maybe (ValueAndDerivativePair2 Number)) -> Array (Maybe Position)
=======
type NumberBounds
  = { upper :: Number, lower :: Number }

toNumberBounds :: Bounds -> NumberBounds
toNumberBounds b = { upper: rationalToNumber b.upper, lower: rationalToNumber b.lower }

plotPoints :: Size -> XYBounds -> NumberBounds -> (Number -> Maybe (ValueAndDerivativePair2 Number)) -> Array Position
>>>>>>> parametric-robust
plotPoints canvasSize bounds domain f = points
  where
  rangeX = rationalToNumber $ bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  width = rationalToNumber canvasSize.width

<<<<<<< HEAD
  domainRange = rationalToNumber $ domain.upper - domain.lower

  numberOfPoints = domainRange

  height = rationalToNumber canvasSize.height

  domainLower = rationalToNumber domain.lower
=======
  domainRange = domain.upper - domain.lower

  height = rationalToNumber canvasSize.height

  domainLower = domain.lower
>>>>>>> parametric-robust

  xLower = rationalToNumber bounds.xBounds.lower

  yLower = rationalToNumber bounds.yBounds.lower

<<<<<<< HEAD
  defaultRange = map (toNumber >>> toDomainX) $ 0 .. (floor numberOfPoints)

  points = map toCanvasPoint defaultRange
=======
  defaultRange = map (toNumber >>> toDomainX) $ 0 .. (floor numberOfDomainPoints)

  evaluatedRange = mapMaybe (\v -> f v <#> (Tuple v)) defaultRange

  points = concat $ zipWith toCanvasPoints evaluatedRange (fromMaybe [] (tail evaluatedRange))
>>>>>>> parametric-robust

  toCanvasX :: Number -> Number
  toCanvasX x = ((x - xLower) * width) / rangeX

  toCanvasY :: Number -> Number
  toCanvasY y = height - (((y - yLower) * height) / rangeY)

  toDomainX :: Number -> Number
<<<<<<< HEAD
  toDomainX canvasX = ((canvasX * domainRange) / numberOfPoints) + domainLower

  toCanvasPoint :: Number -> Maybe Position
  toCanvasPoint t = f t <#> (\{ x, y } -> { x: toCanvasX x.value, y: toCanvasY y.value })

drawPlot :: Array (Maybe Position) -> DrawCommand Unit
=======
  toDomainX canvasX = ((canvasX * domainRange) / numberOfDomainPoints) + domainLower

  toCanvasPoints :: Tuple Number (ValueAndDerivativePair2 Number) -> Tuple Number (ValueAndDerivativePair2 Number) -> Array Position
  toCanvasPoints a b = if shouldSubDivide then plotPoints canvasSize bounds { lower: fst a, upper: fst b } f else [ aPos ]
    where
    aPos = toCanvasPoint $ snd a

    bPos = toCanvasPoint $ snd b

    shouldSubDivide = maxDistanceBetweenPoints < distance aPos bPos

  toCanvasPoint :: ValueAndDerivativePair2 Number -> Position
  toCanvasPoint { x, y } = { x: toCanvasX x.value, y: toCanvasY y.value }

drawPlot :: Array Position -> DrawCommand Unit
>>>>>>> parametric-robust
drawPlot points = for_ lines drawLine
  where
  lines = zipWith Tuple points (fromMaybe [] (tail points))

<<<<<<< HEAD
drawLine :: Tuple (Maybe Position) (Maybe Position) -> DrawCommand Unit
drawLine (Tuple (Just a) (Just b)) = drawPlotLine a b

drawLine (Tuple Nothing Nothing) = pure unit

drawLine (Tuple (Just a) Nothing) = drawPlotLine a a

drawLine (Tuple Nothing (Just b)) = drawPlotLine b b
=======
drawLine :: Tuple Position Position -> DrawCommand Unit
drawLine (Tuple a b) = drawPlotLine a b

distance :: Position -> Position -> Number
distance a b = sqrt $ (y' * y') + (x' * x')
  where
  x' = b.x - a.x

  y' = b.y - a.y
>>>>>>> parametric-robust
