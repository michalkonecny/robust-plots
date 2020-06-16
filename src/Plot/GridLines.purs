module Plot.GridLines where

import Prelude
import Data.Array ((..))
import Data.Decimal as D
import Data.Traversable (for_)
import Draw.Actions (clearCanvas, drawXGridLine, drawYGridLine, drawXAxisLine, drawYAxisLine)
import Draw.Commands (DrawCommand)
import IntervalArith.Misc (Rational, rationalToNumber, toRational)
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
  range = bounds.xBounds.upper - bounds.xBounds.lower

  lineCount = 20

  x1 = bounds.xBounds.lower

  xGuidePoints = map (toGuidePoints x1 range lineCount) $ 0 .. lineCount

  draw :: { component :: Number, value :: Number } -> DrawCommand Unit
  draw { component, value } = drawXGridLine component value (rationalToNumber range)

drawYGridLines :: XYBounds -> DrawCommand Unit
drawYGridLines bounds = for_ yGuidePoints draw
  where
  range = bounds.yBounds.upper - bounds.yBounds.lower

  lineCount = 20

  y1 = bounds.yBounds.lower

  yGuidePoints = map (toGuidePoints y1 range lineCount) $ 0 .. lineCount

  draw :: { component :: Number, value :: Number } -> DrawCommand Unit
  draw { component, value } = drawYGridLine component value (rationalToNumber range)

toGuidePoints :: Rational -> Rational -> Int -> Int -> { component :: Number, value :: Number }
toGuidePoints offset range lineCount index = { component, value }
  where
  numberIndex = toRational index

  componentR = (numberIndex * range) / toRational lineCount

  component = rationalToNumber componentR

  value = to3SignificantDigits $ componentR + offset

toSignificantDigits :: Int -> Rational -> Number
toSignificantDigits digits = D.toNumber <<< (D.toSignificantDigits digits) <<< D.fromNumber <<< rationalToNumber

to3SignificantDigits :: Rational -> Number
to3SignificantDigits = toSignificantDigits 3
