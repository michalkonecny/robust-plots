module Plot.Zoom where

import Prelude

import IntervalArith.Misc (toRational)
import Types (XYBounds)

zoomBounds :: XYBounds -> Boolean -> XYBounds
zoomBounds bounds isZoomIn =
  if isZoomIn then
    bounds
      { xBounds { lower = bounds.xBounds.lower + xMovement, upper = bounds.xBounds.upper - xMovement }
      , yBounds { lower = bounds.yBounds.lower + yMovement, upper = bounds.yBounds.upper - yMovement }
      }
  else
    bounds
      { xBounds { lower = bounds.xBounds.lower - xMovement, upper = bounds.xBounds.upper + xMovement }
      , yBounds { lower = bounds.yBounds.lower - yMovement, upper = bounds.yBounds.upper + yMovement }
      }
  where
  xRange = bounds.xBounds.upper - bounds.xBounds.lower

  yRange = bounds.yBounds.upper - bounds.yBounds.lower

  xMovement = (xRange / toRational 20)

  yMovement = (yRange / toRational 20)
