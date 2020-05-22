module Components.Canvas.Commands.Actions where

import Prelude
import Control.Monad.Free (liftF)
import Components.Canvas.Commands (DrawCommand, DrawCommandF(..))
import Types (Position, Polygon)

clearCanvas :: DrawCommand Unit
clearCanvas = liftF $ ClearCanvas unit

drawText :: String -> Number -> Position -> DrawCommand Unit
drawText text height pos = liftF $ DrawText text height pos unit

drawXGridLine :: Number -> Number -> Number -> DrawCommand Unit
drawXGridLine x value range = liftF $ DrawXGridLine x value range unit

drawYGridLine :: Number -> Number -> Number -> DrawCommand Unit
drawYGridLine y value range = liftF $ DrawYGridLine y value range unit

drawPlotLine :: Position -> Position -> DrawCommand Unit
drawPlotLine a b = liftF $ DrawPlotLine a b unit

drawPolygon :: Polygon -> DrawCommand Unit
drawPolygon polygon = liftF $ DrawPolygon polygon unit

drawEnclosure :: Boolean -> Array Polygon -> DrawCommand Unit
drawEnclosure isSelected polygons = liftF $ DrawEnclosure isSelected polygons unit

drawRootEnclosure :: Number -> Number -> Number -> DrawCommand Unit
drawRootEnclosure yZero l r = liftF $ DrawRootEnclosure yZero l r unit
