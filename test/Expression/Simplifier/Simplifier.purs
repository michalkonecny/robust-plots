module Test.Expression.Simplifier
  ( simplifierTests
  ) where

import Test.Unit (TestSuite)
import Test.Expression.Simplifier.Simplify (simplifyTests)

simplifierTests :: TestSuite
simplifierTests = do
  simplifyTests
