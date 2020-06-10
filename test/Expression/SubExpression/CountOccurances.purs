module Test.Expression.SubExpression.CountOccurances
  ( countOccurancesTests
  ) where

import Prelude
import Data.Either (Either(..))
import Expression.Error (Expect, throw)
import Expression.Parser (parse)
import Expression.SubExpression (SubExpressionCounter, countOccurances)
import Expression.Syntax (Expression)
import Test.Expression.Helper (expectValue)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

countOccurancesTests :: TestSuite
countOccurancesTests =
  suite "Expression.SubExpression - countOccurances" do
    test "ASSERT count occuances correctly WHEN f(x) = sin(x)+sin(x)" do
      let
        -- given
        rawExpression = "sin(x)+sin(x)"

        expected = "[{ expression: (sinx)+(sinx), occurances: 1 },{ expression: sinx, occurances: 2 }]"
      -- when
      expectValue (parseAndMergeCounters rawExpression countOccurances)
        $ \{ counter, expression } -> do
            -- then
            equal expected (show counter)

parseAndMergeCounters :: String -> (Expression -> SubExpressionCounter) -> Expect { counter :: SubExpressionCounter, expression :: Expression }
parseAndMergeCounters rawExpression op = case parse rawExpression of
  Right expression -> pure $ { counter: op expression, expression }
  Left error -> throw error
