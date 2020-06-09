module Test.Expression
  ( expressionTests
  ) where

import Prelude
import Test.Expression.Evaluator (evaluatorTests)
import Test.Expression.Parser (parserTests)
import Test.Expression.Differentiator (differentiatorTests)
import Test.Expression.Simplifier (simplifierTests)
import Test.Expression.VariableMap (variableMapTests)
import Test.Unit (TestSuite)

expressionTests :: TestSuite
expressionTests = do
  parserTests
  evaluatorTests
  differentiatorTests
  simplifierTests
  variableMapTests