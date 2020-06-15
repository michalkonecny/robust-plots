module Test.Expression.SubExpression.RemoveSubExpressions
  ( removeSubExpressionsTests
  ) where

import Prelude

import Data.Either (Either(..))
import Expression.Parser (parse)
import Expression.SubExpression (joinCommonSubExpressions, removeSubExpressions)
import Expression.Syntax (Expression)
import Test.Unit (TestSuite, failure, suite, test)
import Test.Unit.Assert (equal)

removeSubExpressionsTests :: TestSuite
removeSubExpressionsTests =
  suite "Expression.SubExpression - removeSubExpressions" do
    test "SHOULD undo 'joinCommonSubExpressions' WHEN f(x) = sin(sin(sin(x)))+sin(sin(x))+sin(x)" do
      let
        -- given
        rawExpression = "sin(sin(sin(x)))+sin(sin(x))+sin(x)"
      -- when
      case parse rawExpression of
        Left error -> failure $ show error
        Right expression -> equal (show $ wrapAndUnwrap expression) (show expression)
    test "SHOULD undo 'joinCommonSubExpressions' WHEN f(x) = (x+x)+(x+x)" do
      let
        -- given
        rawExpression = "(x+x)+(x+x)"
      -- when
      case parse rawExpression of
        Left error -> failure $ show error
        Right expression -> equal (show $ wrapAndUnwrap expression) (show expression)
    test "SHOULD undo 'joinCommonSubExpressions' WHEN f(x) = x+x" do
      let
        -- given
        rawExpression = "x+x"
      -- when
      case parse rawExpression of
        Left error -> failure $ show error
        Right expression -> equal (show $ wrapAndUnwrap expression) (show expression)
    test "SHOULD undo 'joinCommonSubExpressions' WHEN f(x) = sin((x+x)+(x+x))+(x+x)+sin((x+x)+(x+x))" do
      let
        -- given
        rawExpression = "sin((x+x)+(x+x))+(x+x)+sin((x+x)+(x+x))"
      -- when
      case parse rawExpression of
        Left error -> failure $ show error
        Right expression -> equal (show $ wrapAndUnwrap expression) (show expression)

wrapAndUnwrap :: Expression -> Expression
wrapAndUnwrap = removeSubExpressions <<< joinCommonSubExpressions