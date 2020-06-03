module Draw.Commands where

import Control.Monad.Free (Free)
import Draw.Color (Color)
import Types (Position, Polygon)

type DrawCommand
  = Free DrawCommandF

data DrawCommandF n
  = ClearCanvas n
  | DrawText Color String Number Position n
  | DrawXGridLine Number Number Number n
  | DrawYGridLine Number Number Number n
  | DrawPolygon Polygon n
  | DrawEnclosure Boolean (Array Polygon) n
  | DrawRootEnclosure Number Number Number n
  | DrawPlotLine Position Position n
  | DrawXAxis Number Number n 
  | DrawYAxis Number Number n 
