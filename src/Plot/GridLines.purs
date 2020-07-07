module Plot.GridLines where

import Prelude
import Data.Array ((..))
import Data.Decimal as D
import Data.Foldable (traverse_)
import Data.Int (ceil) as Int
import Data.Int (toNumber)
import Data.Ord (abs)
import Draw.Actions (clearCanvas, drawXGridLine, drawYGridLine, drawXAxisLine, drawYAxisLine)
import Draw.Commands (DrawCommand)
import IntervalArith.Misc (rationalToNumber)
import Math (pow, round, (%))
import Misc.Math (log10)
import Types (XYBounds, Bounds)

clearAndDrawGridLines :: XYBounds -> DrawCommand Unit
clearAndDrawGridLines bounds = do
  clearCanvas
  drawAxes bounds
  drawXGridLines bounds
  drawYGridLines bounds

drawAxes :: XYBounds -> DrawCommand Unit
drawAxes bounds = do
  drawXAxisLine xZero rangeX
  drawYAxisLine yZero rangeY
  where
  xZero = rationalToNumber $ -bounds.xBounds.lower

  yZero = rationalToNumber $ -bounds.yBounds.lower

  rangeX = rationalToNumber $ bounds.xBounds.upper - bounds.xBounds.lower

  rangeY = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

drawXGridLines :: XYBounds -> DrawCommand Unit
drawXGridLines = (traverse_ (draw drawXGridLine)) <<< toGuidePoints <<< (toAxis (_.xBounds))

drawYGridLines :: XYBounds -> DrawCommand Unit
drawYGridLines = (traverse_ (draw drawYGridLine)) <<< toGuidePoints <<< (toAxis (_.yBounds))

draw :: (Number -> Number -> Number -> DrawCommand Unit) -> { component :: Number, value :: Number, range :: Number } -> DrawCommand Unit
draw drawGridLine { component, value, range } = drawGridLine component value range

toGuidePoints :: { range :: Number, interval :: Number, lower :: Number, offset :: Number } -> Array { component :: Number, value :: Number, range :: Number }
toGuidePoints axis@{ range, interval } = map (toNumber >>> (toGuidePoint axis)) $ indexes range interval

toAxis :: (XYBounds -> Bounds) -> XYBounds -> { range :: Number, interval :: Number, lower :: Number, offset :: Number }
toAxis toBounds xyBounds = { range, interval, lower, offset }
  where
  bounds = toBounds xyBounds

  range = rationalToNumber $ bounds.upper - bounds.lower

  interval = calculateInterval range

  lower = rationalToNumber bounds.lower

  offset = lower % interval

toGuidePoint :: { range :: Number, interval :: Number, lower :: Number, offset :: Number } -> Number -> { component :: Number, value :: Number, range :: Number }
toGuidePoint { range, interval, lower, offset } index = { component, value, range }
  where
  c = index * interval

  component = c - offset

  fromLower = component + lower

  value =
    if abs fromLower < interval then
      0.0
    else
      to3SignificantDigits fromLower

toSignificantDigits :: Int -> Number -> Number
toSignificantDigits digits = D.toNumber <<< (D.toSignificantDigits digits) <<< D.fromNumber

to3SignificantDigits :: Number -> Number
to3SignificantDigits = toSignificantDigits 3

calculateInterval :: Number -> Number
calculateInterval range = pow 10.0 $ round $ (log10 range) - 1.0

indexes :: Number -> Number -> Array Int
indexes range interval = 0 .. (Int.ceil $ range / interval)
