module Test.Expression.SubExpression.MergeCounters
  ( mergeCountersTests
  ) where

import Prelude
import Data.Either (Either(..))
import Expression.Error (Expect, throw)
import Expression.Parser (parse)
import Expression.SubExpression (SubExpressionCounter, getOccurances, mergeCounters)
import Expression.Syntax (Expression)
import Test.Expression.Helper (expectValue)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

mergeCountersTests :: TestSuite
mergeCountersTests =
  suite "Expression.SubExpression - mergeCounters" do
    test "ASSERT merged count = 8 WHEN f(x) = sin(x) AND f(x) occurs 3 times in counter 1 AND f(x) occurs 3 times in counter 2" do
      let
        -- given
        rawExpression = "sin(x)"

        count1 = 3

        count2 = 5

        performTestMerge :: Expression -> SubExpressionCounter
        performTestMerge expression = mergeCounters counter1 counter2
          where
          counter1 = [ { expression, occurances: count1 } ]

          counter2 = [ { expression, occurances: count2 } ]

        expectedCount = 8
      -- when
      expectValue (parseAndMergeCounters rawExpression performTestMerge)
        $ \{ mergedCounter, expression } -> do
            -- then
            equal expectedCount (getOccurances mergedCounter expression)
    test "ASSERT merged count = 3 WHEN f(x) = sin(x) AND f(x) occurs 3 times in counter 1 AND f(x) occurs NO times in counter 2" do
      let
        -- given
        rawExpression = "sin(x)"

        count1 = 3

        performTestMerge :: Expression -> SubExpressionCounter
        performTestMerge expression = mergeCounters counter1 counter2
          where
          counter1 = [ { expression, occurances: count1 } ]

          counter2 = [ ] -- Empty

        expectedCount = 3
      -- when
      expectValue (parseAndMergeCounters rawExpression performTestMerge)
        $ \{ mergedCounter, expression } -> do
            -- then
            equal expectedCount (getOccurances mergedCounter expression)

parseAndMergeCounters :: String -> (Expression -> SubExpressionCounter) -> Expect { mergedCounter :: SubExpressionCounter, expression :: Expression }
parseAndMergeCounters rawExpression op = case parse rawExpression of
  Right expression -> pure $ { mergedCounter: op expression, expression }
  Left error -> throw error
