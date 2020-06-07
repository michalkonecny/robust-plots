module Test.IntervalArith.Approx
  ( approxTests
  ) where

import Prelude

import IntervalArith.Approx (Approx(..), approxMB, (⊑))
import IntervalArith.Misc (big)
import Test.IntervalArith.Approx.ShowA (approxTests_showA)
import Test.IntervalArith.Misc (ArbitraryInteger(..))
import Test.Order (partialOrderTests, preOrderTests)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (chooseInt, sized)
import Test.TestUtils (SuiteOrdParams1, assertOpWithInput)
import Test.Unit (TestSuite)

data ArbitraryApprox = ArbitraryApprox Approx

instance arbitraryApprox :: Arbitrary ArbitraryApprox where
  arbitrary =
    sized \size -> do
      (ArbitraryInteger m) <- arbitrary
      s <- chooseInt (-2 * size) (2 * size)
      mb <- chooseInt 2 (10 + 2 * size)
      pure $ ArbitraryApprox $ approxMB mb m (big 0) s

approxTests :: TestSuite
approxTests = do
  approxTests_showA
  approxTests_Order

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
