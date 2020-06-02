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
    test "SHOULD parse as 'x+((2+1)+1)' WHEN input is 'x+(2+1+1)'" do
      let
        -- given
        input = "x+(2+1+1)"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "x+((2+1)+1)"
      equal expectedResult result
    test "SHOULD parse as '1+(sin(x/2))' WHEN input is '1+sin(x/2)'" do
      let
        -- given
        input = "1+sin(x/2)"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "1+(sin(x/2))"
      equal expectedResult result
    test "SHOULD parse as '(sqrt(1+(sin(x/2))))/2' WHEN input is 'sqrt(1+sin(x/2))/2'" do
      let
        -- given
        input = "sqrt(1+sin(x/2))/2"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "(sqrt(1+(sin(x/2))))/2"
      equal expectedResult result
    test "SHOULD fail to parse WHEN input is '1+son(x/2)'" do
      let
        -- given
        input = "1+son(x/2)"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "Parse error: Unknown function: son"
      equal expectedResult result

fromExpect :: Expect Expression -> String
fromExpect (Right expression) = show expression

fromExpect (Left error) = show error
