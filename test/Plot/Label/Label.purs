module Test.Plot.Label
  ( labelTests
  ) where

import Prelude
import Test.Unit (TestSuite)
import Test.Plot.Label.ToRoughLabelPosition (toRoughLabelPositionTests)
import Test.Plot.Label.FixLabelledPositions (fixLabelledPositionsTests)

labelTests :: TestSuite
labelTests = do
  toRoughLabelPositionTests
  fixLabelledPositionsTests
