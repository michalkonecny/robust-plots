module Plot.RobustPlot where

import Prelude
import Data.Array (fold, length, (..), (!!))
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (floor, toNumber)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawEnclosure)
import Draw.Commands (DrawCommand)
import Expression.Evaluator (evaluate)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsNumber, fromInt)
import Plot.Helper (drawLabel)
import Types (Position, Polygon, Size)

type ApproxXYBounds
  = { xBounds :: ApproxBounds, yBounds :: ApproxBounds }

type ApproxBounds
  = { upper :: Approx, lower :: Approx }

type ApproxSize
  = { width :: Approx, height :: Approx }

drawRobustPlot :: Size -> Int -> Int -> ApproxXYBounds -> Expression -> String -> DrawCommand Unit
drawRobustPlot canvasSize numberOfPlots index bounds expression label = drawCommands
  where
  f = evaluateWithX expression

  points = plotPoints canvasSize bounds f

  labelPosition = toPosition $ fromMaybe [ { x: 0.0, y: 0.0 } ] $ points !! ((length points) / ((numberOfPlots + 1) * index))

  drawCommands = fold [ drawPlot points, drawLabel label labelPosition ]

evaluateWithX :: Expression -> Approx -> Approx
evaluateWithX expression x = value
  where
  variableMap = [ Tuple "x" x ]

  value = case evaluate variableMap expression of
    Left _ -> zero
    Right v -> v

plotPoints :: Size -> ApproxXYBounds -> (Approx -> Approx) -> Array Polygon
plotPoints size bounds f = points
  where
  canvasSize = toApproxSize size

  rangeX = bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = bounds.yBounds.upper - bounds.yBounds.lower

  defaultRange = map (fromInt >>> toDomainX) $ 0 .. 50

  points = map toCanvasPoint defaultRange

  toDomainX :: Approx -> Approx
  toDomainX canvasX = ((canvasX * rangeX) / canvasSize.width) + bounds.xBounds.lower

  toCanvasPoint :: Approx -> Polygon
  toCanvasPoint x = polygon
    where
    y = f x

    approxX = ((x - bounds.xBounds.lower) * canvasSize.width) / rangeX

    (Tuple xLower xUpper) = boundsNumber approxX

    approxY = canvasSize.height - (((y - bounds.yBounds.lower) * canvasSize.height) / rangeY)

    (Tuple yLower yUpper) = boundsNumber approxY

    a = { x: xLower, y: yUpper }

    b = { x: xUpper, y: yUpper }

    c = { x: xUpper, y: yLower }

    d = { x: xLower, y: yLower }

    polygon =
      [ a, b, c, d
      ]

drawPlot :: Array Polygon -> DrawCommand Unit
drawPlot ploygons = drawEnclosure true ploygons

toApproxSize :: Size -> ApproxSize
toApproxSize s = { width: fromInt $ floor s.width, height: fromInt $ floor s.height }

toPosition :: Polygon -> Position
toPosition polygon = { x, y }
  where
  x = (sum $ map (\p -> p.x) polygon) / (toNumber $ length polygon)

  y = (sum $ map (\p -> p.y) polygon) / (toNumber $ length polygon)
