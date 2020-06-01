module Components.Canvas.Draw where

import Prelude
import Components.Canvas.Context (DrawOperation, withLocalDrawContext)
import Data.Array (head, tail)
import Data.Maybe (fromMaybe)
import Data.String (joinWith)
import Data.Traversable (for_)
import Effect (Effect)
import Graphics.Canvas (LineCap(..), beginPath, clearRect, fill, fillText, lineTo, moveTo, setFillStyle, setFont, setLineCap, setLineDash, setLineWidth, setStrokeStyle, stroke)
import Types (Polygon, Position, Color)

-- | Draws text
drawText :: String -> Number -> Position -> DrawOperation
drawText text size { x, y } =
  withLocalDrawContext \drawContext -> do
    let
      font = joinWith " " [ show size <> "px", "Arial" ]

      color = { r: 0.0, g: 0.0, b: 0.0, a: 1.0 }
    setFont drawContext.context font
    setFillStyle drawContext.context $ toColorString color
    fillText drawContext.context text x y

clearCanvas :: DrawOperation
clearCanvas =
  withLocalDrawContext \drawContext -> do
    clearRect drawContext.context { height: drawContext.canvasHeight, width: drawContext.canvasWidth, x: 0.0, y: 0.0 }

drawLine :: Boolean -> Color -> Position -> Position -> DrawOperation
drawLine isDashed color { x: x1, y: y1 } { x: x2, y: y2 } =
  withLocalDrawContext \drawContext -> do
    beginPath drawContext.context
    moveTo drawContext.context x1 y1
    lineTo drawContext.context x2 y2
    setStrokeStyle drawContext.context $ toColorString color
    when isDashed do
      setLineDash drawContext.context [ 1.0, 3.0 ]
    stroke drawContext.context

drawPlotLine :: Position -> Position -> DrawOperation
drawPlotLine = drawLine false { r: 0.0, g: 0.0, b: 0.0, a: 1.0 }

drawXAxisLine :: Number -> Number -> DrawOperation
drawXAxisLine xZero range drawContext = drawPlotLine a b drawContext
  where
  relativeX = (xZero * drawContext.canvasWidth) / range

  a = { x: relativeX, y: 0.0 }

  b = { x: relativeX, y: drawContext.canvasHeight }

drawYAxisLine :: Number -> Number -> DrawOperation
drawYAxisLine yZero range drawContext = drawPlotLine a b drawContext
  where
  relativeY = drawContext.canvasHeight - (yZero * drawContext.canvasHeight) / range

  a = { x: 0.0, y: relativeY }

  b = { x: drawContext.canvasWidth, y: relativeY }

drawXGridLine :: Number -> Number -> Number -> DrawOperation
drawXGridLine x value range drawContext = do
  drawLine true color { x: relativeX, y: 0.0 } { x: relativeX, y: drawContext.canvasHeight } drawContext
  drawText (show value) 10.0 { x: relativeX, y: drawContext.canvasHeight - 12.0 } drawContext
  where
  relativeX = (x * drawContext.canvasWidth) / range

  color = { r: 0.0, g: 0.0, b: 0.0, a: 0.3 }

drawYGridLine :: Number -> Number -> Number -> DrawOperation
drawYGridLine y value range drawContext = do
  drawLine true color { x: 0.0, y: relativeY } { x: drawContext.canvasWidth, y: relativeY } drawContext
  drawText (show value) 10.0 { x: drawContext.canvasWidth - 40.0, y: relativeY } drawContext
  where
  relativeY = drawContext.canvasHeight - ((y * drawContext.canvasHeight) / range)

  color = { r: 0.0, g: 0.0, b: 0.0, a: 0.3 }

drawPolygon :: Polygon -> DrawOperation
drawPolygon [] drawContext = pure unit

drawPolygon points drawContext = do
  beginPath drawContext.context
  moveTo drawContext.context x1 y1
  for_ (fromMaybe [] (tail points)) drawPolygonLine
  fill drawContext.context
  stroke drawContext.context
  where
  { x: x1, y: y1 } = fromMaybe origin $ head points

  drawPolygonLine :: Position -> Effect Unit
  drawPolygonLine { x: xi, y: yi } = lineTo drawContext.context xi yi

drawEnclosure :: Boolean -> Array Polygon -> DrawOperation
drawEnclosure isSelected polygons =
  withLocalDrawContext \drawContext -> do
    if isSelected then do
      setFillStyle drawContext.context $ toColorString { r: 255.0, g: 192.0, b: 203.0, a: 0.7 }
      setStrokeStyle drawContext.context $ toColorString { r: 0.0, g: 0.0, b: 0.0, a: 1.0 }
    else do
      setFillStyle drawContext.context $ toColorString { r: 255.0, g: 192.0, b: 203.0, a: 0.4 }
      setStrokeStyle drawContext.context $ toColorString { r: 0.0, g: 0.0, b: 0.0, a: 0.7 }
    for_ polygons (\polygon -> drawPolygon polygon drawContext)

drawRootEnclosure :: Number -> Number -> Number -> DrawOperation
drawRootEnclosure yZero l r =
  withLocalDrawContext \drawContext -> do
    beginPath drawContext.context
    moveTo drawContext.context l yZero
    lineTo drawContext.context r yZero
    setLineWidth drawContext.context 5.0
    setLineCap drawContext.context Round
    setStrokeStyle drawContext.context $ toColorString { r: 255.0, g: 0.0, b: 0.0, a: 1.0 }
    stroke drawContext.context

origin :: Position
origin = { x: 0.0, y: 0.0 }

toColorString :: Color -> String
toColorString { r, g, b, a } = "rgb(" <> (show r) <> "," <> (show g) <> "," <> (show b) <> "," <> (show a) <> ")"
