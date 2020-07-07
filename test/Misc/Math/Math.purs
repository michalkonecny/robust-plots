module Test.Misc.Math
  ( mathTests
  ) where

import Test.Misc.Math.Log10 (log10Tests)
import Test.Unit (TestSuite)

mathTests :: TestSuite
mathTests = do
  log10Tests
