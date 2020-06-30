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
    test "SHOULD split expression into sub expressions WHEN f(x) = sin(x)+sin(x)" do
      let
        -- given
        rawExpression = "sin(x)+sin(x)"

        expected = "[sin(x),(sin(x))+(sin(x))]"
      -- when
      expectValue (parseAndSplitSubExpressions rawExpression splitSubExpressions)
        $ \subExpressions -> do
            -- then
            equal expected (show $ fromFoldable subExpressions)
    test "SHOULD split expression into sub expressions WHEN f(x) = sin(sin(x))+sin(x)" do
      let
        -- given
        rawExpression = "sin(sin(x))+sin(x)"

        expected = "[sin(x),sin(sin(x)),(sin(sin(x)))+(sin(x))]"
      -- when
      expectValue (parseAndSplitSubExpressions rawExpression splitSubExpressions)
        $ \subExpressions -> do
            -- then
            equal expected (show $ fromFoldable subExpressions)

parseAndSplitSubExpressions :: String -> (Expression -> Set Expression) -> Expect (Set Expression)
parseAndSplitSubExpressions rawExpression op = case parse rawExpression of
  Right expression -> pure $ op expression
  Left error -> throw error
