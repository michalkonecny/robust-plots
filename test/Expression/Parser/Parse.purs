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
    test "SHOULD parse as '5' WHEN input is '5'" do
      let
        -- given
        input = "5"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "5"
      equal expectedResult result
    test "SHOULD parse as '0.1' WHEN input is '0.1'" do
      let
        -- given
        input = "0.1"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "0.1"
      equal expectedResult result
    test "SHOULD parse as '0.001' WHEN input is '0.001'" do
      let
        -- given
        input = "0.001"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "0.001"
      equal expectedResult result
    test "SHOULD parse as '0.123' WHEN input is '0.123'" do
      let
        -- given
        input = "0.123"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "0.123"
      equal expectedResult result
    test "SHOULD parse as '-2.1' WHEN input is '-2.1'" do
      let
        -- given
        input = "-2.1"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "-2.1"
      equal expectedResult result
    test "SHOULD parse as '-0.123' WHEN input is '-0.123'" do
      let
        -- given
        input = "-0.123"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "-0.123"
      equal expectedResult result
    test "SHOULD parse as '0.123' WHEN input is '+0.123'" do
      let
        -- given
        input = "+0.123"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "0.123"
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
    test "SHOULD parse as '1-(2*3)' WHEN input is '1-2*3'" do
      let
        -- given
        input = "1-2*3"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "1-(2*3)"
      equal expectedResult result
    test "SHOULD parse as '1+(2*3)' WHEN input is '1+2*3'" do
      let
        -- given
        input = "1+2*3"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "1+(2*3)"
      equal expectedResult result
    test "SHOULD parse as '1+(2*3)' WHEN input is '1+(2)*(3)'" do
      let
        -- given
        input = "1+(2)*(3)"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "1+(2*3)"
      equal expectedResult result
    test "SHOULD parse as '1-((x/6)*(1-(x/20)))' WHEN input is '1-(x/6)*(1-(x/20))'" do
      let
        -- given
        input = "1-(x/6)*(1-(x/20))"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "1-((x/6)*(1-(x/20)))"
      equal expectedResult result
    test "SHOULD parse as '(x+2)+(3+4)' WHEN input is '(x+2)+(3+4)'" do
      let
        -- given
        input = "(x+2)+(3+4)"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "(x+2)+(3+4)"
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
