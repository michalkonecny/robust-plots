module Plot.Pan where

import Prelude
import Types (Direction(..), Position, XYBounds, Size)

panBounds :: XYBounds -> Direction -> XYBounds
panBounds bounds = case _ of
  Left -> bounds { xBounds { lower = bounds.xBounds.lower - xMovement, upper = bounds.xBounds.upper - xMovement } }
  Right -> bounds { xBounds { lower = bounds.xBounds.lower + xMovement, upper = bounds.xBounds.upper + xMovement } }
  Up -> bounds { yBounds { lower = bounds.yBounds.lower + yMovement, upper = bounds.yBounds.upper + yMovement } }
  Down -> bounds { yBounds { lower = bounds.yBounds.lower - yMovement, upper = bounds.yBounds.upper - yMovement } }
  where
  xRange = bounds.xBounds.upper - bounds.xBounds.lower

  yRange = bounds.yBounds.upper - bounds.yBounds.lower

  xMovement = (xRange / 10.0)

  yMovement = (yRange / 10.0)

panBoundsByVector :: Size -> XYBounds -> Position -> XYBounds
panBoundsByVector canvasSize bounds delta =
  { xBounds: { lower: bounds.xBounds.lower + xMovement, upper: bounds.xBounds.upper + xMovement }
  , yBounds: { lower: bounds.yBounds.lower + yMovement, upper: bounds.yBounds.upper + yMovement }
  }
  where
  xRange = bounds.xBounds.upper - bounds.xBounds.lower

  yRange = bounds.yBounds.upper - bounds.yBounds.lower

  xMovement = (delta.x * xRange) / canvasSize.width

  yMovement = (delta.y * yRange) / canvasSize.height
