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
    test "SHOULD fail to parse WHEN input is 'x+/2'" do
      let
        -- given
        input = "x+/2"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "Parse error: Invalid operator location"
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
    test "SHOULD parse as 'sin(x)' WHEN input is 'sin(x)'" do
      let
        -- given
        input = "sin(x)"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "sin(x)"
      equal expectedResult result
    test "SHOULD parse as 'e^x' WHEN input is 'exp(x)'" do
      let
        -- given
        input = "exp(x)"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "e^x"
      equal expectedResult result
    test "SHOULD parse as 'e^5' WHEN input is 'exp(5)'" do
      let
        -- given
        input = "exp(5)"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "e^5"
      equal expectedResult result
    test "SHOULD parse as 'cos(x)' WHEN input is 'cos(x)'" do
      let
        -- given
        input = "cos(x)"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "cos(x)"
      equal expectedResult result
    test "SHOULD parse as 'tan(x)' WHEN input is 'tan(x)'" do
      let
        -- given
        input = "tan(x)"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "tan(x)"
      equal expectedResult result
    test "SHOULD parse as 'log(x)' WHEN input is 'log(x)'" do
      let
        -- given
        input = "log(x)"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "log(x)"
      equal expectedResult result
    test "SHOULD parse as 'sqrt(x)' WHEN input is 'sqrt(x)'" do
      let
        -- given
        input = "sqrt(x)"

        -- when
        result = fromExpect $ parse input

        -- then
        expectedResult = "sqrt(x)"
      equal expectedResult result

fromExpect :: Expect Expression -> String
fromExpect (Right expression) = show expression

fromExpect (Left error) = show error
