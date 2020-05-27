module Test.Expression.Parser.Parse
  ( parseTests
  ) where

import Prelude
import Data.Either (Either(..))
import Expression.Error (Expect)
import Expression.Parser (parse)
import Expression.Syntax (Expression)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

parseTests :: TestSuite
parseTests =
  suite "Expression.Parser - parse" do
    test "SHOULD parse as 'x' WHEN input is 'x'" do
      let
        -- given
        input = "x"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "x"
      equal expectedResult result
    test "SHOULD parse as 'x+y' WHEN input is 'x+y'" do
      let
        -- given
        input = "x+y"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "x+y"
      equal expectedResult result

fromExpect :: Expect Expression -> String
fromExpect (Right expression) = show expression

fromExpect (Left error) = show error
