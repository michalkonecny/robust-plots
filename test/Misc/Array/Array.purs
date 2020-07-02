module Test.Misc.Array
  ( arrayTests
  ) where

import Test.Misc.Array.Split (splitTests)
import Test.Unit (TestSuite)

arrayTests :: TestSuite
arrayTests = do
  splitTests
