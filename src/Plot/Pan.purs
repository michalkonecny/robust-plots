module Plot.Pan where

import Prelude
import Types (Direction(..), XYBounds)

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
