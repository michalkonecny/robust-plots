module Types where

type Domain
  = { xBounds :: Bounds, yBounds :: Bounds }

type Bounds
  = { upper :: Number, lower :: Number }

type Position
  = { x :: Number, y :: Number }

type Polygon = Array Position
