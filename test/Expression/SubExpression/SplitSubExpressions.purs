module Test.Expression.SubExpression.SplitSubExpressions
  ( splitSubExpressionsTests
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Set (Set)
import Expression.Error (Expect, throw)
import Expression.Parser (parse)
import Expression.SubExpression (splitSubExpressions)
import Expression.Syntax (Expression)
import Test.Expression.Helper (expectValue)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

splitSubExpressionsTests :: TestSuite
splitSubExpressionsTests =
  suite "Expression.SubExpression - splitSubExpressions" do
    test "ASSERT count occuances correctly WHEN f(x) = sin(x)+sin(x)" do
      let
        -- given
        rawExpression = "sin(x)+sin(x)"

        expected = "[sinx,(sinx)+(sinx)]"
      -- when
      expectValue (parseAndMergeCounters rawExpression splitSubExpressions)
        $ \subExpressions -> do
            -- then
            equal expected (show $ fromFoldable subExpressions)

parseAndMergeCounters :: String -> (Expression -> Set Expression) -> Expect (Set Expression)
parseAndMergeCounters rawExpression op = case parse rawExpression of
  Right expression -> pure $ op expression
  Left error -> throw error
