module Test.Expression.Parser.FoldIntoRational
  ( foldIntoRationalTests
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.List (List, fromFoldable)
import Data.Ratio ((%))
import Expression.Parser (foldIntoRational)
import IntervalArith.Misc (Rational, Integer, big)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

foldIntoRationalTests :: TestSuite
foldIntoRationalTests =
  suite "Expression.Parser - foldIntoRational" do
    test "SHOULD fold into 0.123 WHEN input is [1,2,3] and base is 0" do
      let
        -- given
        input = toList [ 1, 2, 3 ]

        base = zero

        -- when
        result = foldIntoRational base input

        -- then
        expectedResult = ratio 123 1000
      equal expectedResult result
    test "SHOULD fold into 0.1 WHEN input is [1] and base is 0" do
      let
        -- given
        input = toList [ 1 ]

        base = zero

        -- when
        result = foldIntoRational base input

        -- then
        expectedResult = ratio 1 10
      equal expectedResult result
    test "SHOULD fold into 0.01 WHEN input is [0,1] and base is 0" do
      let
        -- given
        input = toList [ 0, 1 ]

        base = zero

        -- when
        result = foldIntoRational base input

        -- then
        expectedResult = ratio 1 100
      equal expectedResult result

ratio :: Int -> Int -> Rational
ratio n d = (big n) % (big d)

toList :: Array Int -> List Integer
toList = fromFoldable <<< (map big)