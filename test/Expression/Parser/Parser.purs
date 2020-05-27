module Test.Expression.Parser
  ( parserTests
  ) where

import Test.Unit (TestSuite)
import Test.Expression.Parser.Parse (parseTests)

parserTests :: TestSuite
parserTests = do
  parseTests