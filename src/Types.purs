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

type RGBA
  = { r :: Number
    , g :: Number
    , b :: Number
    , a :: Number
    }

data Direction
  = Up
  | Down
  | Left
  | Right
