module Test.Expression.SubExpression.SubstituteSubExpressions
  ( substituteSubExpressionsTests
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Map (Map, values)
import Data.Map.Internal (keys)
import Expression.Error (Expect, throw)
import Expression.Parser (parse)
import Expression.SubExpression (splitSubExpressions, subExpressionToVariableMap, substituteSubExpressions)
import Expression.Syntax (Expression, VariableName)
import Test.Expression.Helper (expectValue)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

substituteSubExpressionsTests :: TestSuite
substituteSubExpressionsTests =
  suite "Expression.SubExpression - substituteSubExpressions" do
    test "SHOULD substitue sub expressions WHEN f(x) = sin(x)+sin(x)" do
      let
        -- given
        rawExpression = "sin(x)+sin(x)"

        expectedKeys = "[sin(x),$v1+$v1]"
        expectedValues = "[\"$v1\",\"$v2\"]"
      -- when
      expectValue (parseAndSubstituteSubExpressions rawExpression (substituteSubExpressions <<< subExpressionToVariableMap <<< splitSubExpressions))
        $ \subExpressions -> do
            -- then
            equal expectedKeys (show $ fromFoldable $ keys subExpressions)
            equal expectedValues (show $ fromFoldable $ values subExpressions)
    test "SHOULD substitue sub expressions WHEN f(x) = sin(sin(x))+sin(x)" do
      let
        -- given
        rawExpression = "sin(sin(x))+sin(x)"

        expectedKeys = "[sin($v1),sin(x),$v2+$v1]"
        expectedValues = "[\"$v2\",\"$v1\",\"$v3\"]"
      -- when
      expectValue (parseAndSubstituteSubExpressions rawExpression (substituteSubExpressions <<< subExpressionToVariableMap <<< splitSubExpressions))
        $ \subExpressions -> do
            -- then
            equal expectedKeys (show $ fromFoldable $ keys subExpressions)
            equal expectedValues (show $ fromFoldable $ values subExpressions)

parseAndSubstituteSubExpressions :: String -> (Expression -> Map Expression VariableName) -> Expect (Map Expression VariableName)
parseAndSubstituteSubExpressions rawExpression op = case parse rawExpression of
  Right expression -> pure $ op expression
  Left error -> throw error
