module Test.Misc
  ( miscTests
  ) where

import Prelude
import Test.Misc.Array (arrayTests)
import Test.Misc.Math (mathTests)
import Test.Unit (TestSuite)

miscTests :: TestSuite
miscTests = do
  arrayTests
  mathTests
