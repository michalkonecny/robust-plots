module Misc.Math where

import Prelude
import Math (log, log10e)

-- | Calculates the log to base 10 of the given `Number`. log10(x) = ln(x) * ln(e)
log10 :: Number -> Number
log10 v = (log v) * log10e
