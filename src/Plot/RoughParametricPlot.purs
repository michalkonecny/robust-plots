module Plot.RoughParametricPlot where

import Prelude
import Data.Array (tail, zipWith, (..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawPlotLine)
import Draw.Commands (DrawCommand)
import Expression.Error (expectToMaybe)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative2, evaluateDerivative2)
import Expression.Syntax (Expression)
import IntervalArith.Misc (rationalToNumber)
import Math (e, pi)
import Types (Position, Size, XYBounds, Bounds)

drawRoughParametricPlot :: Size -> XYBounds -> Bounds -> Expression -> Expression -> DrawCommand Unit
drawRoughParametricPlot canvasSize bounds domain xExpression yExpression = drawCommands
  where
  f = evaluateWithT xExpression yExpression

  points = plotPoints canvasSize bounds domain f

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

  result = case evaluator xExpression, evaluator xExpression of
    Just x, Just y -> Just { x, y }
    _, _ -> Nothing

plotPoints :: Size -> XYBounds -> Bounds -> (Number -> Maybe (ValueAndDerivativePair2 Number)) -> Array (Maybe Position)
plotPoints canvasSize bounds domain f = points
  where
  rangeX = rationalToNumber $ bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  width = rationalToNumber canvasSize.width

  domainRange = rationalToNumber $ domain.upper - domain.lower

  numberOfPoints = domainRange / 2.0

  height = rationalToNumber canvasSize.height

  domainLower = rationalToNumber domain.lower

  xLower = rationalToNumber bounds.xBounds.lower

  yLower = rationalToNumber bounds.yBounds.lower

  defaultRange = map (toNumber >>> toDomainX) $ 0 .. (floor numberOfPoints)

  points = map toCanvasPoint defaultRange

  toCanvasX :: Number -> Number
  toCanvasX x = ((x - xLower) * width) / rangeX

  toCanvasY :: Number -> Number
  toCanvasY y = height - (((y - yLower) * height) / rangeY)

  toDomainX :: Number -> Number
  toDomainX canvasX = ((canvasX * domainRange) / numberOfPoints) + domainLower

  toCanvasPoint :: Number -> Maybe Position
  toCanvasPoint t = f t <#> (\{ x, y } -> { x: toCanvasX x.value, y: toCanvasY y.value })

drawPlot :: Array (Maybe Position) -> DrawCommand Unit
drawPlot points = for_ lines drawLine
  where
  lines = zipWith Tuple points (fromMaybe [] (tail points))

drawLine :: Tuple (Maybe Position) (Maybe Position) -> DrawCommand Unit
drawLine (Tuple (Just a) (Just b)) = drawPlotLine a b

drawLine (Tuple Nothing Nothing) = pure unit

drawLine (Tuple (Just a) Nothing) = drawPlotLine a a

drawLine (Tuple Nothing (Just b)) = drawPlotLine b b
