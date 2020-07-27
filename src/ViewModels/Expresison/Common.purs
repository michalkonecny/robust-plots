module ViewModels.Expression.Common where

import Prelude

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
