module Plotters where

import Prelude
import Math (pow)

plot1 :: Number -> Number
plot1 x = 1.0 / (1.0 + (100.0 * pow x 2.0))
