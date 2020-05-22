module Components.Canvas.PlotController where

import Prelude
import Components.Canvas.Commands (DrawCommand)
import Components.Canvas.Commands.Actions (drawXGridLine, drawYGridLine, drawPolygon)
import Components.Canvas.Plot (Plot(..))
import Data.Array ((..))
import Data.Decimal as D
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (floor, toNumber)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff, nonCanceler)
import Types (XYBounds)

computePlotAsync :: Plot -> Aff (DrawCommand Unit)
computePlotAsync plot = makeAff $ runComputation plot

runComputation :: Plot -> (Either Error (DrawCommand Unit) -> Effect Unit) -> Effect Canceler
runComputation (Polygon bounds polygon) callback = do
  callback $ Right
    $ do
        -- Computation for drawing plot here
        drawGridlines bounds
        drawPolygon polygon
  pure nonCanceler

drawGridlines :: XYBounds -> DrawCommand Unit
drawGridlines bounds = do
  drawXGridLines bounds
  drawYGridLines bounds

drawXGridLines :: XYBounds -> DrawCommand Unit
drawXGridLines bounds = for_ xGuidePoints draw
  where
  range = bounds.xBounds.upper - bounds.xBounds.lower

  lineCount = 20.0

  x1 = bounds.xBounds.lower

  xGuidePoints = map (toGuidePoints x1 range lineCount) $ 0 .. (floor lineCount)

  draw :: { component :: Number, value :: Number } -> DrawCommand Unit
  draw { component, value } = drawXGridLine component value range

drawYGridLines :: XYBounds -> DrawCommand Unit
drawYGridLines bounds = for_ yGuidePoints draw
  where
  range = bounds.yBounds.upper - bounds.yBounds.lower

  lineCount = 20.0

  y1 = bounds.yBounds.lower

  yGuidePoints = map (toGuidePoints y1 range lineCount) $ 0 .. (floor lineCount)

  draw :: { component :: Number, value :: Number } -> DrawCommand Unit
  draw { component, value } = drawYGridLine component value range

toGuidePoints :: Number -> Number -> Number -> Int -> { component :: Number, value :: Number }
toGuidePoints offset range lineCount index = { component, value }
  where
  numberIndex = toNumber index

  component = (numberIndex * range) / lineCount

  value = to3SignificantDigits $ component + offset

toSignificantDigits :: Int -> Number -> Number
toSignificantDigits digits = D.toNumber <<< (D.toSignificantDigits digits) <<< D.fromNumber

to3SignificantDigits :: Number -> Number
to3SignificantDigits = toSignificantDigits 3
