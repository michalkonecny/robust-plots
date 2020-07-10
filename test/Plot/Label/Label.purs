module Test.Plot.Label
  ( labelTests
  ) where

import Test.Unit (TestSuite)
import Test.Plot.Label.ToRoughLabelPosition (toRoughLabelPositionTests)

labelTests :: TestSuite
labelTests = do
  toRoughLabelPositionTests
