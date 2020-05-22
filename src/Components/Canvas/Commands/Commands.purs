module Components.Canvas.Commands where

import Control.Monad.Free (Free)
import Types (Position, Polygon)

type DrawCommand
  = Free DrawCommandF

data DrawCommandF n
  = ClearCanvas n
  | DrawText String Number Position n
  | DrawXGridLine Number Number Number n
  | DrawYGridLine Number Number Number n
  | DrawPolygon Polygon n
  | DrawEnclosure Boolean (Array Polygon) n
  | DrawRootEnclosure Number Number Number n
  | DrawPlotLine Position Position n
