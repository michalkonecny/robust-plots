module Components.Canvas.PlotController where

import Prelude
import Components.Canvas.Commands (DrawCommand)
import Components.Canvas.Commands.Actions (drawXGridLine, drawYGridLine, drawPolygon)
import Components.Canvas.Plot (Plot(..))
import Data.Array ((..))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (floor, toNumber, ceil)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff, nonCanceler)
import Math (log, pow, round)
import Types (XYBounds)

computePlotAsync :: Plot -> Aff (DrawCommand Unit)
computePlotAsync plot = makeAff $ runComputation plot

runComputation :: Plot -> (Either Error (DrawCommand Unit) -> Effect Unit) -> Effect Canceler
runComputation (Polygon bounds polygon) callback = do
  callback $ Right
    $ do
        -- Computation for drawing plot here
        drawXGridLines bounds
        drawYGridLines bounds
        drawPolygon polygon
  pure nonCanceler

drawXGridLines :: XYBounds -> DrawCommand Unit
drawXGridLines bounds = for_ xGuidePoints draw
  where
  range = bounds.xBounds.upper - bounds.xBounds.lower
  interval = range / 20.0
  x1 = bounds.xBounds.lower
  xGuidePoints = map (\p -> (((toNumber p) * range) / 20.0) + x1) $ 0 .. 20

  draw :: Number -> DrawCommand Unit
  draw xi = drawXGridLine xiD xi range
    where
    xiD = xi - bounds.xBounds.lower

drawYGridLines :: XYBounds -> DrawCommand Unit
drawYGridLines bounds = for_ yGuidePoints draw
  where
  range = bounds.yBounds.upper - bounds.yBounds.lower
  gran = pow 10.0 $ log $ range / 10.0

  y1 = bounds.yBounds.lower

  yGuidePoints = map (\p -> ((toNumber p) * gran) + y1) $ 0 .. (ceil (range / gran))

  draw :: Number -> DrawCommand Unit
  draw yi = drawYGridLine yiD yi range
    where
    yiD = yi - bounds.yBounds.lower
