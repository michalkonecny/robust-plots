module Test.Expression.Parser
  ( parserTests
  ) where

import Prelude
import Test.Unit (TestSuite)
import Test.Expression.Parser.Parse (parseTests)
import Test.Expression.Parser.FoldIntoRational (foldIntoRationalTests)

parserTests :: TestSuite
parserTests = do
  parseTests
  foldIntoRationalTests