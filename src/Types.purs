module Types where

type XYBounds
  = { xBounds :: Bounds, yBounds :: Bounds }

type Bounds
  = { upper :: Number, lower :: Number }

type Position
  = { x :: Number, y :: Number }

type Size
  = { width :: Number, height :: Number }

type Polygon
  = Array Position

data Direction = Up | Down | Left | Right
