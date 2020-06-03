module Test.Expression.Differentiator
  ( differentiatorTests
  ) where

import Test.Unit (TestSuite)
import Test.Expression.Differentiator.Differentiate (differentiateTests)

differentiatorTests :: TestSuite
differentiatorTests = do
  differentiateTests
