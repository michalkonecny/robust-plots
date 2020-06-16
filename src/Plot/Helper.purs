module Plot.Helper where

import Prelude
import Draw.Actions (drawText)
import Draw.Color (rgba)
import Draw.Commands (DrawCommand)
import Types (Position)

drawLabel :: String -> Position -> DrawCommand Unit
drawLabel label labelPosition = drawText color ("f(x)=" <> label) 20.0 labelPosition
  where
  color = rgba 255.0 0.0 0.0 1.0

  
