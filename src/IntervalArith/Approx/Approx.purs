{-
  Based on CDAR.Approx fork by Michal Konečný (https://github.com/michalkonecny/cdar)
  originally developed by Jens Blanck (https://github.com/jensblanck/cdar).
-}
module IntervalArith.Approx (module IntervalArith.Approx.Type, module IntervalArith.Approx.ShowA) where

import IntervalArith.Approx.Type (Approx(..), Precision, approxAutoMB, approxMB, approxMB2, centreA, enforceMB, exact, lowerA, mBound, mapMB, setMB, upperA)
import IntervalArith.Approx.ShowA (showA, showExactA, showInBaseA, showInexactA, showNearZeroA, toDigit, unsafeToDigit)
