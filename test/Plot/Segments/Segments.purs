module Test.Plot.Segments
  ( segmentsTests
  ) where

import Test.Unit (TestSuite)
import Test.Plot.Segments.SegmentDomain (segmentDomainTests)

segmentsTests :: TestSuite
segmentsTests = do
  segmentDomainTests
