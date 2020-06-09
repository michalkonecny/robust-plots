module Test.Expression.VariableMap.Lookup
  ( lookUpTests
  ) where

import Prelude
import Expression.VariableMap (VariableMap, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

lookUpTests :: TestSuite
lookUpTests =
  suite "Expression.VariableMap - lookup" do
    test "SHOULD not find value WHEN value does not exist" do
      let
        -- given
        variables :: VariableMap Number
        variables = []

        variableName = "a"

        -- when
        result = show $ lookup variables variableName

        -- then
        expectedResult = "Nothing"
      equal expectedResult result
    test "SHOULD find value WHEN value not exist" do
      let
        -- given
        variables :: VariableMap Number
        variables = [ (Tuple "a" 0.1) ]

        variableName = "a"

        -- when
        result = case lookup variables variableName of
          Just value -> show value
          Nothing -> "Nothing"

        -- then
        expectedResult = "0.1"
      equal expectedResult result
