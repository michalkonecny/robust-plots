module Test.IntervalArith.Approx
  ( approxTests, ArbitraryApprox(..), randomSampleApprox
  ) where

import Prelude
import Effect (Effect)
import IntervalArith.Approx (Approx(..), approxMB, consistent, setMB, (⊑))
import IntervalArith.Approx.ShowA (showA)
import IntervalArith.Misc (big)
import Test.IntervalArith.Approx.ShowA (approxTests_showA)
import Test.IntervalArith.Misc (ArbitraryInteger(..), ArbitraryPositiveExponent(..))
import Test.Order (preOrderTests)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Size, chooseInt, randomSample', sized)
import Test.TestUtils (SuiteOrdParams1, assertOp, assertOpWithInput)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

data ArbitraryApprox
  = ArbitraryApprox Approx

instance arbitraryApprox :: Arbitrary ArbitraryApprox where
  arbitrary =
    sized \size -> do
      (ArbitraryInteger m) <- arbitrary
      s <- chooseInt (-2 * size) (2 * size)
      mb <- chooseInt 10 (10 + 2 * size)
      pure $ ArbitraryApprox $ approxMB mb m (big 0) s

randomSampleApprox :: Size -> Effect (Array String)
randomSampleApprox size = randomSample' size (map (\(ArbitraryApprox a) -> a) arbitrary) >>= \as -> pure (map showA as)

approxTests :: TestSuite
approxTests = do
  approxTests_showA
  approxTests_setMBworse
  approxTests_Order
  approxTests_Consistent

approxTests_setMBworse :: TestSuite
approxTests_setMBworse =
  suite "IntervalArith.Approx - setMB" do
    test "SHOULD HOLD setMB mb a ⊑ a FOR ALL approx a and integer mb>=0"
      $ quickCheck \aPre mbPre ->
          let
            -- given
            (ArbitraryApprox a) = aPre

            (ArbitraryPositiveExponent mb) = mbPre

            -- when
            aMB = setMB mb a
          -- then
          in
            assertOp (⊑) " ⊑ " aMB a

approxTests_Order :: TestSuite
approxTests_Order =
  suite "IntervalArith.Approx - approximation order (`better`)" do
    test "SHOULD give (Approx 4 12 1 3 = 96±8 \"1~~\") WHEN setMB 4 (Approx 10 100 0 0 ~ \"100\")" do
      let
        -- given
        input = Approx 10 (big 100) (big 0) 0

        -- when
        result = Approx 4 (big 12) (big 1) 3

        -- then
        expected = result
      equal expected result
    preOrderTests approxOrdParams

approxTests_Consistent :: TestSuite
approxTests_Consistent =
  suite "IntervalArith.Approx - consistency check (`consistent`)" do
    test "SHOULD HOLD consistent (Approx 4 12 1 3 = 96±8) (Approx 4 13 1 3 = 104±8)" do
      let
        -- given
        input1 = Approx 4 (big 12) (big 1) 3

        input2 = Approx 4 (big 12) (big 1) 3

        -- when
        result = consistent input1 input2
      -- then
      equal result true
    test "SHOULD HOLD consistent a b WHEN a ⊑ b FOR ALL approx a b"
      $ quickCheck \aPre bPre ->
          let
            -- given
            (ArbitraryApprox b) = bPre

            (ArbitraryApprox a1) = aPre

            -- when
            a = approxOrdParams.makeLeq b a1
          -- then
          in
            assertOp consistent " `consistent` " a b

approxOrdParams :: SuiteOrdParams1 ArbitraryApprox Approx
approxOrdParams =
  { suitePrefix: "IntervalArith.Approx ⊑"
  , valuesName: "interval approximations"
  , fromArbitraryValue: \(ArbitraryApprox d) -> d
  , leqOpWithInput: (assertOpWithInput (⊑) " ⊑ ")
  , leqOpSymbol: "⊑"
  , eqOpWithInput: (assertOpWithInput (==) " == ")
  , eqOpSymbol: "="
  , makeLeq:
      \a b -> case b of
        (Approx mb _ _ _) -> setMB mb a
        Bottom -> Bottom
  }
