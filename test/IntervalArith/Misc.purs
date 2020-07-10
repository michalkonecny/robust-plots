module Test.IntervalArith.Misc where

import Prelude
import Data.Foldable (foldl)
import Data.NonEmpty ((:|))
import Data.Ord (abs)
import Data.Ratio ((%))
import IntervalArith.Misc (Integer, Rational, big, scale)
import Test.QuickCheck (class Arbitrary, arbitrary, (<=?), (==?))
import Test.QuickCheck.Combinators ((&=&))
import Test.QuickCheck.Gen (chooseInt, elements, listOf, sized)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

miscTests :: TestSuite
miscTests =
  suite "IntervalArith.Misc - scale Integer" do
    test "SHOULD HOLD n = scale (scale n i) (-i) FOR ALL integers n and i>=0"
      $ quickCheck \nPre iPre ->
          let
            -- given
            (ArbitraryInteger n) = nPre

            (ArbitraryPositiveExponent i) = iPre

            -- when
            result = scale (scale n i) (-i)

            -- then
            expected = n
          in
            expected ==? result
    test "SHOULD HOLD n <= 2*(scale n (-1)) <= (n+1) FOR ALL integers n"
      $ quickCheck \nPre ->
          let
            -- given
            (ArbitraryInteger n) = nPre

            -- when
            result = (big 2) * (scale n (-1))

            -- then
            expected = n
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

newtype ArbitraryPositiveExponent
  = ArbitraryPositiveExponent Int

instance arbitraryPositiveExponent :: Arbitrary ArbitraryPositiveExponent where
  arbitrary =
    sized \size ->
      ArbitraryPositiveExponent <$> chooseInt 0 (2 * size + 1)

newtype ArbitraryInt0To1000
  = ArbitraryInt0To1000 Int

instance arbitraryInt0To1000 :: Arbitrary ArbitraryInt0To1000 where
  arbitrary = ArbitraryInt0To1000 <$> chooseInt 0 1000

newtype ArbitraryRational
  = ArbitraryRational Rational

instance arbitraryRational :: Arbitrary ArbitraryRational where
  arbitrary = do
    (ArbitraryInteger p) <- arbitrary
    (ArbitraryInteger qPre) <- arbitrary
    pure $ ArbitraryRational $ p % (one + (abs qPre))
