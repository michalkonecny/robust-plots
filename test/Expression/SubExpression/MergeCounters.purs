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

          counter2 = [] -- Empty

        expectedCount = 3
      -- when
      expectValue (parseAndMergeCounters rawExpression performTestMerge)
        $ \{ mergedCounter, expression } -> do
            -- then
            equal expectedCount (getOccurances mergedCounter expression)
    test "ASSERT merged count = 5 WHEN f(x) = sin(x) AND f(x) occurs 5 times in counter 2 AND f(x) occurs NO times in counter 1" do
      let
        -- given
        rawExpression = "sin(x)"

        count2 = 5

        performTestMerge :: Expression -> SubExpressionCounter
        performTestMerge expression = mergeCounters counter1 counter2
          where
          counter1 = [] -- Empty

          counter2 = [ { expression, occurances: count2 } ]

        expectedCount = 5
      -- when
      expectValue (parseAndMergeCounters rawExpression performTestMerge)
        $ \{ mergedCounter, expression } -> do
            -- then
            equal expectedCount (getOccurances mergedCounter expression)
    test "ASSERT merged count to be correct WHEN given two counters with overlapping expression counts" do
      let
        rawExpression1 = "sin(x)"

        rawExpression2 = "tan(x)"

        countExpression1InCounter1 = 3

        countExpression1InCounter2 = 5

        countExpression2InCounter2 = 3

        expectedCount1 = 8

        expectedCount2 = 3

        buildCounter1 :: Expression -> SubExpressionCounter
        buildCounter1 expression = [ { expression, occurances: countExpression1InCounter1 } ]
      expectValue (parseAndMergeCounters rawExpression1 buildCounter1)
        $ \{ mergedCounter: counter1, expression: expression1 } -> do
            let
              buildCounter2 :: Expression -> SubExpressionCounter
              buildCounter2 expression2 = [ { expression: expression1, occurances: countExpression1InCounter2 }, { expression: expression2, occurances: countExpression2InCounter2 } ]
            expectValue (parseAndMergeCounters rawExpression2 buildCounter2)
              $ \{ mergedCounter: counter2, expression: expression2 } -> do
                  let
                    mergedCounter = mergeCounters counter1 counter2
                  equal expectedCount1 (getOccurances mergedCounter expression1)
                  equal expectedCount2 (getOccurances mergedCounter expression2)

parseAndMergeCounters :: String -> (Expression -> SubExpressionCounter) -> Expect { mergedCounter :: SubExpressionCounter, expression :: Expression }
parseAndMergeCounters rawExpression op = case parse rawExpression of
  Right expression -> pure $ { mergedCounter: op expression, expression }
  Left error -> throw error
