module Test.Misc
  ( miscTests
  ) where

import Test.Misc.Array (arrayTests)
import Test.Unit (TestSuite)

miscTests :: TestSuite
miscTests = do
  arrayTests
