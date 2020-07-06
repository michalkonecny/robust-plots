module Plot.GridLines where

import Prelude

import Data.Array ((..))
import Data.Decimal as D
import Data.Int (toNumber)
import Data.Traversable (for_)
import Draw.Actions (clearCanvas, drawXGridLine, drawYGridLine, drawXAxisLine, drawYAxisLine)
import Draw.Commands (DrawCommand)
import IntervalArith.Misc (rationalToNumber)
import Math (pow, round)
import Misc.Math (log10)
import Data.Int (ceil) as Int
import Types (XYBounds)

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
drawXGridLines bounds = for_ xGuidePoints draw
  where
  range = rationalToNumber $ bounds.xBounds.upper - bounds.xBounds.lower

  interval = step range

  x1 = to3SignificantDigits $ rationalToNumber bounds.xBounds.lower

  xGuidePoints = map (toGuidePoints x1 range interval) $ indexes range interval

  draw :: { component :: Number, value :: Number } -> DrawCommand Unit
  draw { component, value } = drawXGridLine component value range

drawYGridLines :: XYBounds -> DrawCommand Unit
drawYGridLines bounds = for_ yGuidePoints draw
  where
  range = rationalToNumber $ bounds.yBounds.upper - bounds.yBounds.lower

  interval = step range

  y1 = to3SignificantDigits $ rationalToNumber bounds.yBounds.lower

  yGuidePoints = map (toGuidePoints y1 range interval) $ indexes range interval

  draw :: { component :: Number, value :: Number } -> DrawCommand Unit
  draw { component, value } = drawYGridLine component value range

toGuidePoints :: Number -> Number -> Number -> Int -> { component :: Number, value :: Number }
toGuidePoints offset range interval index = { component, value }
  where
  numberIndex = toNumber index

  component = numberIndex * interval

  value = to3SignificantDigits $ component + offset

toSignificantDigits :: Int -> Number -> Number
toSignificantDigits digits = D.toNumber <<< (D.toSignificantDigits digits) <<< D.fromNumber

to3SignificantDigits :: Number -> Number
to3SignificantDigits = toSignificantDigits 3
 
step :: Number -> Number
step range =  pow 10.0 $ round $ (log10 range) - 1.0

indexes :: Number -> Number -> Array Int
indexes range interval = 0 .. (Int.ceil $ range / interval)