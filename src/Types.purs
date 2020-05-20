module Types where

type Domain
  = { xBounds :: RangeBounds, yBounds :: RangeBounds }

type RangeBounds
  = { upper :: Number, lower :: Number }

type Position
  = { x :: Number, y :: Number }

type Size
  = { width :: Number, height :: Number }

type Polygon
  = Array Position
