module Components.Canvas.Draw where

import Prelude
import Data.Traversable (for_)
import Data.Array (head, tail)
import Data.Maybe (fromMaybe)
import Data.String (joinWith)
import Effect (Effect)
import Graphics.Canvas (LineCap(..), beginPath, clearRect, fill, fillText, lineTo, moveTo, setFillStyle, setFont, setLineCap, setLineDash, setLineWidth, setStrokeStyle, stroke)
import Types (Position, Polygon)
import Components.Canvas.Context (DrawOperation, withLocalDrawContext)

-- | Draws text
drawText :: String -> Number -> Position -> DrawOperation
drawText text size { x, y } =
  withLocalDrawContext \drawContext -> do
    let
      font = joinWith " " [ show size <> "px", "Arial" ]
    -- Set up the context for drawing the text
    setFont drawContext.context font
    setFillStyle drawContext.context "#000000"
    -- Draw the text
    fillText drawContext.context text x y

clearCanvas :: DrawOperation
clearCanvas =
  withLocalDrawContext \drawContext -> do
    clearRect drawContext.context { height: drawContext.canvasHeight, width: drawContext.canvasWidth, x: 0.0, y: 0.0 }

drawGridLine :: Position -> Position -> DrawOperation
drawGridLine { x: x1, y: y1 } { x: x2, y: y2 } =
  withLocalDrawContext \drawContext -> do
    beginPath drawContext.context
    moveTo drawContext.context x1 y1
    lineTo drawContext.context x2 y2
    setLineDash drawContext.context [ 1.0, 3.0 ]
    setStrokeStyle drawContext.context $ toRGBA 0.0 0.0 0.0 0.3
    stroke drawContext.context

drawPolygon :: Polygon -> DrawOperation
drawPolygon [] drawContext = pure unit

drawPolygon points drawContext = do
  let
    { x: x1, y: y1 } = fromMaybe origin $ head points
  beginPath drawContext.context
  moveTo drawContext.context x1 y1
  for_ (fromMaybe [] (tail points)) drawLine
  fill drawContext.context
  stroke drawContext.context
  where
  drawLine :: Position -> Effect Unit
  drawLine { x: xi, y: yi } = lineTo drawContext.context xi yi

drawEnclosure :: Boolean -> Array Polygon -> DrawOperation
drawEnclosure isSelected polygons =
  withLocalDrawContext \drawContext -> do
    if isSelected then do
      setFillStyle drawContext.context $ toRGBA 255.0 192.0 203.0 0.7
      setStrokeStyle drawContext.context $ toRGBA 0.0 0.0 0.0 1.0
    else do
      setFillStyle drawContext.context $ toRGBA 255.0 192.0 2013.0 0.4
      setStrokeStyle drawContext.context $ toRGBA 0.0 0.0 0.0 0.7
    for_ polygons (\polygon -> drawPolygon polygon drawContext)

drawRootEnclosure :: Number -> Number -> Number -> DrawOperation
drawRootEnclosure yZero l r =
  withLocalDrawContext \drawContext -> do
    beginPath drawContext.context
    moveTo drawContext.context l yZero
    lineTo drawContext.context r yZero
    setLineWidth drawContext.context 5.0
    setLineCap drawContext.context Round
    setStrokeStyle drawContext.context $ toRGBA 255.0 0.0 0.0 1.0
    stroke drawContext.context

origin :: Position
origin = { x: 0.0, y: 0.0 }

toRGBA :: Number -> Number -> Number -> Number -> String
toRGBA r g b a = "rgb(" <> (show r) <> "," <> (show g) <> "," <> (show b) <> "," <> (show a) <> ")"