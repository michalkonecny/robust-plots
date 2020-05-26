module Test.IntervalArith.Misc where

import Prelude
import Data.Foldable (foldl)
import Data.NonEmpty ((:|))
import IntervalArith.Misc (Integer, big, scale)
import Test.QuickCheck (class Arbitrary, (<=?), (==?))
import Test.QuickCheck.Gen (chooseInt, elements, listOf, sized)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck.Combinators ((&=&))

miscTests :: TestSuite
miscTests =
  suite "IntervalArith.Misc - scale Integer" do
    test "SHOULD HOLD n = scale (scale n 1) (-1) FOR ALL integers n"
      $ quickCheck \n ->
          let
            -- given
            (ArbitraryInteger input) = n :: ArbitraryInteger

            -- when
            result = scale (scale input 1) (-1)

            -- then
            expected = input
          in
            expected ==? result
    test "SHOULD HOLD n <= 2*(scale n (-1)) <= (n+1) FOR ALL integers n"
      $ quickCheck \n ->
          let
            -- given
            (ArbitraryInteger input) = n :: ArbitraryInteger

            -- when
            result = (big 2) * (scale input (-1))

            -- then
            expected = input
          in
            (expected <=? result) &=& (result <=? expected + (big 1))

newtype ArbitraryInteger
  = ArbitraryInteger Integer

instance arbitraryInteger :: Arbitrary ArbitraryInteger where
  arbitrary = do
    sign <- elements $ 0 :| [ 1, -1 ]
    n <- sized nonneg
    pure $ ArbitraryInteger $ (big sign) * n
    where
    nonneg 0 = big <$> chooseInt 0 1

    nonneg 1 = big <$> chooseInt 0 1024

    nonneg 2 = big <$> chooseInt 0 (1024 * 1024)

    nonneg 3 = big <$> chooseInt 0 (1024 * 1024 * 1024)

    nonneg size = do
      nSize <- chooseInt 1 size
      ns <- listOf ((nSize + 1) `div` 3) (chooseInt 0 (top - 1))
      let
        n = foldl (\b a -> b * top + a) 0 ns
      pure $ big n
