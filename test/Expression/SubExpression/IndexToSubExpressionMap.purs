module Test.Expression.SubExpression.IndexToSubExpressionMap
  ( indexToSubExpressionMapTests
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Map (Map, values)
import Data.Map.Internal (keys)
import Expression.Error (Expect, throw)
import Expression.Parser (parse)
import Expression.SubExpression (splitSubExpressions, indexToSubExpressionMap)
import Expression.Syntax (Expression, VariableName)
import Test.Expression.Helper (expectValue)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

indexToSubExpressionMapTests :: TestSuite
indexToSubExpressionMapTests =
  suite "Expression.SubExpression - indexToSubExpressionMap" do
    test "SHOULD assign variable names to sub expressions WHEN f(x) = sin(x)+sin(x)" do
      let
        -- given
        rawExpression = "sin(x)+sin(x)"

        expectedKeys = "[sinx,(sinx)+(sinx)]"
        expectedValues = "[\"$v1\",\"$v2\"]"
      -- when
      expectValue (parseAndMergeCounters rawExpression (indexToSubExpressionMap <<< splitSubExpressions))
        $ \subExpressions -> do
            -- then
            equal expectedKeys (show $ fromFoldable $ keys subExpressions)
            equal expectedValues (show $ fromFoldable $ values subExpressions)
    test "SHOULD assign variable names to sub expressions WHEN f(x) = sin(sin(x))+sin(x)" do
      let
        -- given
        rawExpression = "sin(sin(x))+sin(x)"

        expectedKeys = "[sinx,sin(sinx),(sin(sinx))+(sinx)]"
        expectedValues = "[\"$v1\",\"$v2\",\"$v3\"]"
      -- when
      expectValue (parseAndMergeCounters rawExpression (indexToSubExpressionMap <<< splitSubExpressions))
        $ \subExpressions -> do
            -- then
            equal expectedKeys (show $ fromFoldable $ keys subExpressions)
            equal expectedValues (show $ fromFoldable $ values subExpressions)

parseAndMergeCounters :: String -> (Expression -> Map Expression VariableName) -> Expect (Map Expression VariableName)
parseAndMergeCounters rawExpression op = case parse rawExpression of
  Right expression -> pure $ op expression
  Left error -> throw error
