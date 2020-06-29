module Plot.RoughPlot where

import Prelude
import Data.Array (concat, tail, zipWith, (..))
import Data.Either (Either(..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawPlotLine)
import Draw.Commands (DrawCommand)
import Expression.Differentiator (secondDifferentiate)
import Expression.Evaluator (roughEvaluate, presetConstants)
import Expression.Simplifier (simplify)
import Expression.Syntax (Expression)
import IntervalArith.Misc (rationalToNumber)
import Math (abs)
import Types (Size, XYBounds, Position)

drawRoughPlot :: Size -> XYBounds -> Expression -> String -> DrawCommand Unit
drawRoughPlot canvasSize bounds expression label = drawCommands
  where
  f = evaluateWithX expression

  f'' = (evaluateWithX <<< simplify <<< secondDifferentiate) expression

  points = plotPoints canvasSize bounds f f''

  drawCommands = drawPlot points

evaluateWithX :: Expression -> Number -> Maybe Number
evaluateWithX expression x = value
  where
  variableMap = presetConstants <> [ Tuple "x" x ]

  value = case roughEvaluate variableMap expression of
    Left _ -> Nothing
    Right v -> Just v

plotPoints :: Size -> XYBounds -> (Number -> Maybe Number) -> (Number -> Maybe Number) -> Array (Maybe Position)
plotPoints canvasSize bounds f f'' = points
  where
  rangeX = rationalToNumber $ bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  width = rationalToNumber canvasSize.width

  height = rationalToNumber canvasSize.height

  xLower = rationalToNumber bounds.xBounds.lower

  yLower = rationalToNumber bounds.yBounds.lower

  defaultRange = map (toNumber >>> toDomainX) $ 0 .. (floor width)

  changeInGradient = map f'' defaultRange

  points = map toCanvasPoint $ concat $ zipWith toRange changeInGradient defaultRange

  toRange :: Maybe Number -> Number -> Array Number
  toRange Nothing value = [ value ]

  toRange (Just deltaGradient) value =
    if (abs deltaGradient) < (width / rangeX) then
      [ value ]
    else
      map (toNumber >>> toSubRange) $ -5 .. 5
    where
    toSubRange :: Number -> Number
    toSubRange x = value + ((x * rangeX) / (width * 10.0))

  toCanvasX :: Number -> Number
  toCanvasX x = ((x - xLower) * width) / rangeX

  toCanvasY :: Number -> Number
  toCanvasY y = height - (((y - yLower) * height) / rangeY)

  toDomainX :: Number -> Number
  toDomainX canvasX = ((canvasX * rangeX) / width) + xLower

  toCanvasPoint :: Number -> Maybe Position
  toCanvasPoint x = case f x of
    Just y -> Just { x: toCanvasX x, y: toCanvasY y }
    Nothing -> Nothing

drawPlot :: Array (Maybe Position) -> DrawCommand Unit
drawPlot points = for_ lines drawLine
  where
  lines = zipWith Tuple points (fromMaybe [] (tail points))

drawLine :: Tuple (Maybe Position) (Maybe Position) -> DrawCommand Unit
drawLine (Tuple (Just a) (Just b)) = drawPlotLine a b

drawLine (Tuple Nothing Nothing) = pure unit

drawLine (Tuple (Just a) Nothing) = drawPlotLine a a

drawLine (Tuple Nothing (Just b)) = drawPlotLine b b
