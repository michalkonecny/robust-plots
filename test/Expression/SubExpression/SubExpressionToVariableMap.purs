module Test.Expression.SubExpression.SubExpressionToVariableMap
  ( subExpressionToVariableMapTests
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Map (Map, values)
import Data.Map.Internal (keys)
import Expression.Error (Expect, throw)
import Expression.Parser (parse)
import Expression.SubExpression (splitSubExpressions, subExpressionToVariableMap)
import Expression.Syntax (Expression, VariableName)
import Test.Expression.Helper (expectValue)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

subExpressionToVariableMapTests :: TestSuite
subExpressionToVariableMapTests =
  suite "Expression.SubExpression - subExpressionToVariableMap" do
    test "SHOULD assign variable names to sub expressions WHEN f(x) = sin(x)+sin(x)" do
      let
        -- given
        rawExpression = "sin(x)+sin(x)"

        expectedKeys = "[sinx,(sinx)+(sinx)]"
        expectedValues = "[\"$v1\",\"$v2\"]"
      -- when
      expectValue (parseAndBuildSubExpresionMap rawExpression (subExpressionToVariableMap <<< splitSubExpressions))
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
      expectValue (parseAndBuildSubExpresionMap rawExpression (subExpressionToVariableMap <<< splitSubExpressions))
        $ \subExpressions -> do
            -- then
            equal expectedKeys (show $ fromFoldable $ keys subExpressions)
            equal expectedValues (show $ fromFoldable $ values subExpressions)

parseAndBuildSubExpresionMap :: String -> (Expression -> Map Expression VariableName) -> Expect (Map Expression VariableName)
parseAndBuildSubExpresionMap rawExpression op = case parse rawExpression of
  Right expression -> pure $ op expression
  Left error -> throw error
