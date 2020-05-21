module Components.Canvas.Plot where

import Types (Polygon)

data Plot
  = Ploygon Polygon

basicPolygon :: Plot
basicPolygon = Ploygon polygon
  where
  p1 = { x: 0.0, y: 0.0 }

  p2 = { x: 20.0, y: 0.0 }

  p3 = { x: 20.0, y: 20.0 }

  p4 = { x: 0.0, y: 20.0 }

  polygon = [ p1, p2, p3, p4 ]
