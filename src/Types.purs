module Types where

import IntervalArith.Misc (Rational)

type XYBounds
  = { xBounds :: Bounds, yBounds :: Bounds }

type Bounds
  = { upper :: Rational, lower :: Rational }

type Position
  = { x :: Number, y :: Number }

type Size
  = { width :: Rational, height :: Rational }

type Polygon
  = Array Position

type Delta
  = { x :: Rational, y :: Rational }

data Direction
  = Up
  | Down
  | Left
  | Right