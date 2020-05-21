module Components.Canvas.PlotController where

import Prelude
import Components.Canvas.Commands (DrawCommand)
import Components.Canvas.Commands.Actions (drawPolygon)
import Components.Canvas.Plot (Plot(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff, nonCanceler)

computePlotAsync :: Plot -> Aff (DrawCommand Unit)
computePlotAsync plot = makeAff $ runComputation plot

runComputation :: Plot -> (Either Error (DrawCommand Unit) -> Effect Unit) -> Effect Canceler
runComputation (Ploygon polygon) callback = do
  callback $ Right
    $ do
        -- Computation for drawing plot here
        drawPolygon polygon
  pure nonCanceler
