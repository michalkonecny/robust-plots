module Test.IntervalArith.Approx
  ( approxTests, ArbitraryApprox(..), randomSampleApprox
  ) where

import Prelude

import Effect (Effect)
import IntervalArith.Approx (Approx, approxMB, better, setMB, (⊑))
import IntervalArith.Approx.ShowA (showA)
import IntervalArith.Misc (big)
import Test.IntervalArith.Approx.ShowA (approxTests_showA)
import Test.IntervalArith.Misc (ArbitraryInteger(..), ArbitraryPositiveExponent(..))
import Test.Order (preOrderTests)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Size, chooseInt, randomSample', sized)
import Test.TestUtils (SuiteOrdParams1, assertOp, assertOpWithInput)
import Test.Unit (TestSuite, suite, test)
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

approxTests_setMBworse :: TestSuite
approxTests_setMBworse =
  suite "IntervalArith.Approx - setMB" do
    test "SHOULD HOLD setMB mb a ⊒ a FOR ALL approx a and integer mb>=0"
      $ quickCheck \aPre mbPre ->
          let
            -- given
            (ArbitraryApprox a) = aPre

            (ArbitraryPositiveExponent mb) = mbPre

            -- when
            aMB = setMB mb a
          -- then
          in
            assertOp (flip better) " ⊒ " aMB a

approxTests_Order :: TestSuite
approxTests_Order = preOrderTests approxOrdParams

approxOrdParams :: SuiteOrdParams1 ArbitraryApprox Approx
approxOrdParams =
  { suitePrefix: "IntervalArith.Approx ⊑"
  , valuesName: "interval approximations"
  , fromArbitraryValue: \(ArbitraryApprox d) -> d
  , leqOpWithInput: (assertOpWithInput (⊑) " ⊑ ")
  , leqOpSymbol: "⊑"
  , eqOpWithInput: (assertOpWithInput (==) " == ")
  , eqOpSymbol: "="
  }
