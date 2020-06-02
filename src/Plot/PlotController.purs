module Plot.PlotController where

import Prelude
import Data.Array (fold, tail, zipWith, (..), concat)
import Data.Either (Either(..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawPlotLine)
import Draw.Commands (DrawCommand)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff, nonCanceler)
import Expression.Differentiator (secondDifferentiate)
import Expression.Evaluator (evaluate, presetConstants)
import Expression.Helper (simplify)
import Expression.Syntax (Expression)
import Plot.Commands (PlotCommand(..))
import Plot.GridLines (clearAndDrawGridLines)
import Types (Size, XYBounds, Position)

computePlotAsync :: Size -> Array PlotCommand -> Aff (DrawCommand Unit)
computePlotAsync canvasSize plot = makeAff $ runComputation canvasSize plot

runComputation :: Size -> Array PlotCommand -> (Either Error (DrawCommand Unit) -> Effect Unit) -> Effect Canceler
runComputation canvasSize commands callback = do
  callback $ Right result
  pure nonCanceler
  where
  result = fold $ map (runCommand canvasSize) commands

evaluateWithX :: Expression -> Number -> Number
evaluateWithX expression x = value
  where
  variableMap = presetConstants <> [ Tuple "x" x ]

  value = case evaluate variableMap expression of
    Left _ -> 0.0
    Right v -> v

runCommand :: Size -> PlotCommand -> DrawCommand Unit
runCommand canvasSize (Empty bounds) = clearAndDrawGridLines bounds

runCommand canvasSize (Plot bounds expression) = plotSimpleLine canvasSize bounds f f''
  where
  f = evaluateWithX expression

  f'' = evaluateWithX $ simplify $ secondDifferentiate expression

plotSimpleLine :: Size -> XYBounds -> (Number -> Number) -> (Number -> Number) -> DrawCommand Unit
plotSimpleLine canvasSize bounds f f'' = for_ lines (\l -> drawPlotLine l.a l.b)
  where
  rangeX = bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = bounds.yBounds.upper - bounds.yBounds.lower

  defaultRange = map (toNumber >>> toDomainX) $ 0 .. (floor canvasSize.width)

  changeInGradient = map f'' defaultRange

  points = map toCanvasPoint $ concat $ zipWith toRange changeInGradient defaultRange

  lines = zipWith (\a b -> { a, b }) points (fromMaybe [] (tail points))

  toRange :: Number -> Number -> Array Number
  toRange deltaGradient value =
    if deltaGradient < (canvasSize.width / rangeX) then
      [ value ]
    else
      map (accurate <<< toNumber) $ -10 .. 10
    where
    accurate :: Number -> Number
    accurate x = value + ((x * rangeX) / (canvasSize.width * 10.0))

  toCanvasX :: Number -> Number
  toCanvasX x = ((x - bounds.xBounds.lower) * canvasSize.width) / rangeX

  toDomainX :: Number -> Number
  toDomainX canvasX = ((canvasX * rangeX) / canvasSize.width) + bounds.xBounds.lower

  toCanvasPoint :: Number -> Position
  toCanvasPoint x = { x: toCanvasX x, y: canvasY }
    where
    y = f x

    canvasY = canvasSize.height - (((y - bounds.yBounds.lower) * canvasSize.height) / rangeY)

toMaybePlotCommand :: PlotCommand -> Maybe PlotCommand
toMaybePlotCommand (Empty _) = Nothing

toMaybePlotCommand p = Just p
