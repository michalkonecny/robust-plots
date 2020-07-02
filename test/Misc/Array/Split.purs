module Test.Misc.Array.Split
  ( splitTests
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Misc.Array (split)
import Data.Array (length, (!!), (..))
import Test.Unit (TestSuite, failure, suite, test)
import Test.Unit.Assert (equal)

splitTests :: TestSuite
splitTests =
  suite "Misc.Array - split" do
    test "SHOULD each element in seperate array WHEN splitCount = array length" do
      let
        array = 1 .. 5

        numberOfSplits = 5

        splitArray = split numberOfSplits array
      equal numberOfSplits (length splitArray)
      checkElement [ 1 ] (splitArray !! 0)
      checkElement [ 2 ] (splitArray !! 1)
      checkElement [ 3 ] (splitArray !! 2)
      checkElement [ 4 ] (splitArray !! 3)
      checkElement [ 5 ] (splitArray !! 4)
    test "SHOULD each element in seperate array WHEN splitCount > array length" do
      let
        array = 1 .. 5

        numberOfSplits = 6

        splitArray = split numberOfSplits array
      equal 5 (length splitArray)
      checkElement [ 1 ] (splitArray !! 0)
      checkElement [ 2 ] (splitArray !! 1)
      checkElement [ 3 ] (splitArray !! 2)
      checkElement [ 4 ] (splitArray !! 3)
      checkElement [ 5 ] (splitArray !! 4)
    test "SHOULD seperate elements correctly WHEN splitCount < array length AND array = [1 .. 5]" do
      let
        array = 1 .. 5

        numberOfSplits = 4

        splitArray = split numberOfSplits array
      equal numberOfSplits (length splitArray)
      checkElement [ 1 ] (splitArray !! 0)
      checkElement [ 2 ] (splitArray !! 1)
      checkElement [ 3 ] (splitArray !! 2)
      checkElement [ 4, 5 ] (splitArray !! 3)
    test "SHOULD seperate elements correctly WHEN splitCount = 4 AND array = [1 .. 8]" do
      let
        array = 1 .. 8

        numberOfSplits = 4

        splitArray = split numberOfSplits array
      equal numberOfSplits (length splitArray)
      checkElement [ 1, 2 ] (splitArray !! 0)
      checkElement [ 3, 4 ] (splitArray !! 1)
      checkElement [ 5, 6 ] (splitArray !! 2)
      checkElement [ 7, 8 ] (splitArray !! 3)
    test "SHOULD seperate elements correctly WHEN splitCount = 4 AND array = [1 .. 8]" do
      let
        array = 1 .. 9

        numberOfSplits = 3

        splitArray = split numberOfSplits array
      equal numberOfSplits (length splitArray)
      checkElement [ 1, 2, 3 ] (splitArray !! 0)
      checkElement [ 4, 5, 6 ] (splitArray !! 1)
      checkElement [ 7, 8, 9 ] (splitArray !! 2)


checkElement :: forall a. Show a => Eq a => a -> Maybe a -> Aff Unit
checkElement expected Nothing = failure $ "Expected " <> (show expected) <> " but got Nothing"

checkElement expected (Just value) = equal expected value
