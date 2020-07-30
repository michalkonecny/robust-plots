module Plot.RoughFunctionPlot where

import Prelude
import Data.Array (concat, tail, zipWith, (..))
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
import Math (abs, e, pi)
import Types (Size, XYBounds, Position)

drawRoughPlot :: Size -> XYBounds -> Expression -> DrawCommand Unit
drawRoughPlot canvasSize bounds expression = drawCommands
  where
  f = evaluateWithX expression

  points = plotPoints canvasSize bounds f

  drawCommands = drawPlot points

evaluateWithX :: Expression -> Number -> Maybe (ValueAndDerivative2 Number)
evaluateWithX expression x = expectToMaybe $ evaluateDerivative2 variableMap expression
  where
  variableMap =
    [ Tuple "x" { value: x, derivative: 1.0, derivative2: 0.0 }
    , Tuple "e" { value: e, derivative: 0.0, derivative2: 0.0 }
    , Tuple "pi" { value: pi, derivative: 0.0, derivative2: 0.0 }
    ]

plotPoints :: Size -> XYBounds -> (Number -> Maybe (ValueAndDerivative2 Number)) -> Array (Maybe Position)
plotPoints canvasSize bounds f = points
  where
  rangeX = rationalToNumber $ bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  width = rationalToNumber canvasSize.width

  numberOfPoints = width / 2.0

  height = rationalToNumber canvasSize.height

  xLower = rationalToNumber bounds.xBounds.lower

  yLower = rationalToNumber bounds.yBounds.lower

  defaultRange = map (toNumber >>> toDomainX) $ 0 .. (floor numberOfPoints)

  changeInGradient = map toChangeInGradient defaultRange

  points = map toCanvasPoint $ concat $ zipWith toRange changeInGradient defaultRange

  toRange :: Maybe Number -> Number -> Array Number
  toRange Nothing value = [ value ]

  toRange (Just deltaGradient) value =
    if (abs deltaGradient) < (width / rangeX) then
      [ value ]
    else
      map (toNumber >>> toSubRange) $ -2 .. 2
    where
    toSubRange :: Number -> Number
    toSubRange x = value + ((x * rangeX) / (numberOfPoints * 4.0))

  toCanvasX :: Number -> Number
  toCanvasX x = ((x - xLower) * width) / rangeX

  toCanvasY :: Number -> Number
  toCanvasY y = height - (((y - yLower) * height) / rangeY)

  toDomainX :: Number -> Number
  toDomainX canvasX = ((canvasX * rangeX) / numberOfPoints) + xLower

  toChangeInGradient :: Number -> Maybe Number
  toChangeInGradient x = f x <#> (_.derivative2)

  toCanvasPoint :: Number -> Maybe Position
  toCanvasPoint x = f x <#> (\y -> { x: toCanvasX x, y: toCanvasY y.value })

drawPlot :: Array (Maybe Position) -> DrawCommand Unit
drawPlot points = for_ lines drawLine
  where
  lines = zipWith Tuple points (fromMaybe [] (tail points))

drawLine :: Tuple (Maybe Position) (Maybe Position) -> DrawCommand Unit
drawLine (Tuple (Just a) (Just b)) = drawPlotLine a b

drawLine (Tuple Nothing Nothing) = pure unit

drawLine (Tuple (Just a) Nothing) = drawPlotLine a a

drawLine (Tuple Nothing (Just b)) = drawPlotLine b b
