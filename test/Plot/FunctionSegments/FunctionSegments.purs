module Test.Plot.FunctionSegments
  ( segmentsTests
  ) where

import Test.Unit (TestSuite)
import Test.Plot.FunctionSegments.SegmentDomain (segmentDomainTests)

segmentsTests :: TestSuite
segmentsTests = do
  segmentDomainTests
