module Test.Expression.VariableMap
  ( variableMapTests
  ) where

import Test.Expression.VariableMap.Lookup (lookUpTests)
import Test.Unit (TestSuite)

variableMapTests :: TestSuite
variableMapTests = do
  lookUpTests