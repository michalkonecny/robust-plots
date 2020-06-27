module Test.Expression.SubExpression.OrderDepencencies
  ( orderDepencenciesTests
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple, fst, snd)
import Expression.Error (Expect, throw)
import Expression.Parser (parse)
import Expression.SubExpression (splitSubExpressions, subExpressionToVariableMap, substituteSubExpressions, orderDepencencies)
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
      expectValue (parseAndOrderDependencies rawExpression pipeline)
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
      expectValue (parseAndOrderDependencies rawExpression pipeline)
        $ \subExpressions -> do
            -- then
            equal expectedKeys (show $ map fst subExpressions)
            equal expectedValues (show $ map snd subExpressions)
    test "ASSERT order sub expression dependencies WHEN f(x) = 1-(x/20)" do
      let
        -- given
        rawExpression = "1-(x/20)"

        expectedKeys = "[x/20,1-$v2]"

        expectedValues = "[\"$v2\",\"$v1\"]"
      -- when
      expectValue (parseAndOrderDependencies rawExpression pipeline)
        $ \subExpressions -> do
            -- then
            equal expectedKeys (show $ map fst subExpressions)
            equal expectedValues (show $ map snd subExpressions)
    test "ASSERT order sub expression dependencies WHEN f(x) = 1-(x/6)*(1-(x/20))" do
      let
        -- given
        rawExpression = "1-(x/6)*(1-(x/20))"

        expectedKeys = "[x/6,x/20,1-$v5,$v4*$v2,1-$v3]"

        expectedValues = "[\"$v4\",\"$v5\",\"$v2\",\"$v3\",\"$v1\"]"
      -- when
      expectValue (parseAndOrderDependencies rawExpression pipeline)
        $ \subExpressions -> do
            -- then
            equal expectedKeys (show $ map fst subExpressions)
            equal expectedValues (show $ map snd subExpressions)

pipeline :: Expression -> Array (Tuple Expression VariableName)
pipeline = orderDepencencies <<< substituteSubExpressions <<< subExpressionToVariableMap <<< splitSubExpressions

parseAndOrderDependencies :: String -> (Expression -> Array (Tuple Expression VariableName)) -> Expect (Array (Tuple Expression VariableName))
parseAndOrderDependencies rawExpression op = case parse rawExpression of
  Right expression -> pure $ op expression
  Left error -> throw error
