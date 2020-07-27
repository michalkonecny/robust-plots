module ViewModels.Expression.Common where

import Prelude
import Draw.Commands (DrawCommand)
import IntervalArith.Misc (rationalToNumber)
import Types (XYBounds, Size)

type AccuracyCalculator
  = Number -> Number

data DrawingStatus
  = DrawnRough
  | RobustInProgress
  | DrawnRobust
  | DrawnNone

derive instance drawingStatusEq :: Eq DrawingStatus

data Status
  = Off
  | Rough
  | Robust

derive instance statusEq :: Eq Status

type DrawingCommands
  = { robust :: DrawCommand Unit
    , rough :: DrawCommand Unit
    , status :: DrawingStatus
    }

fromPixelAccuracy :: Size -> XYBounds -> Number -> Number
fromPixelAccuracy canvasSize bounds pixelAccuracy = pixelAccuracy * pixelToDomainRatio
  where
  rangeY = bounds.yBounds.upper - bounds.yBounds.lower

  pixelToDomainRatio = rationalToNumber $ rangeY / canvasSize.height
