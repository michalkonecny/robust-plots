module Test.Expression.SubExpression.OrderDepencencies
  ( orderDepencenciesTests
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple, fst, snd)
import Expression.Error (Expect, throw)
import Expression.Parser (parse)
import Expression.SubExpression (splitSubExpressions, indexToSubExpressionMap, substituteSubExpressions, orderDepencencies)
import Expression.Syntax (Expression, VariableName)
import Test.Expression.Helper (expectValue)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

orderDepencenciesTests :: TestSuite
orderDepencenciesTests =
  suite "Expression.SubExpression - orderDepencencies" do
    test "ASSERT order sub expression dependencies WHEN f(x) = sin(x)+sin(x)" do
      let
        -- given
        rawExpression = "sin(x)+sin(x)"

        expectedKeys = "[sinx,$v1+$v1]"

        expectedValues = "[\"$v1\",\"$v2\"]"
      -- when
      expectValue (parseAndMergeCounters rawExpression (orderDepencencies <<< substituteSubExpressions <<< indexToSubExpressionMap <<< splitSubExpressions))
        $ \subExpressions -> do
            -- then
            equal expectedKeys (show $ map fst subExpressions)
            equal expectedValues (show $ map snd subExpressions)
    test "ASSERT order sub expression dependencies WHEN f(x) = sin(sin(x))+sin(x)" do
      let
        -- given
        rawExpression = "sin(sin(x))+sin(x)"

        expectedKeys = "[sinx,sin$v1,$v2+$v1]"

        expectedValues = "[\"$v1\",\"$v2\",\"$v3\"]"
      -- when
      expectValue (parseAndMergeCounters rawExpression (orderDepencencies <<< substituteSubExpressions <<< indexToSubExpressionMap <<< splitSubExpressions))
        $ \subExpressions -> do
            -- then
            equal expectedKeys (show $ map fst subExpressions)
            equal expectedValues (show $ map snd subExpressions)

parseAndMergeCounters :: String -> (Expression -> Array (Tuple Expression VariableName)) -> Expect (Array (Tuple Expression VariableName))
parseAndMergeCounters rawExpression op = case parse rawExpression of
  Right expression -> pure $ op expression
  Left error -> throw error
